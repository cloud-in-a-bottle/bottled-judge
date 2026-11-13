exception Http of string * string

let fail status message = raise (Http (status, message))
let env key = try Sys.getenv key with Not_found -> ""
let data () = Sys.getenv "OPENHOST_APP_DATA_DIR"
let path = Filename.concat
let valid s = String.length s > 0 && String.for_all (function
  | 'a'..'z' | 'A'..'Z' | '0'..'9' | '-' | '_' -> true | _ -> false) s

let segment s = if not (valid s) then fail "404 Not Found" "Not found" else s
let base token = "/c/" ^ token
let html title body = View.page title body
let text = "text/plain; charset=utf-8"
let markup = "text/html; charset=utf-8"

let plain () =
  List.exists (fun value -> String.starts_with ~prefix:"text/plain" (String.trim value))
    (String.split_on_char ',' (String.lowercase_ascii (env "HTTP_ACCEPT")))

let error status message =
  if plain () then View.send status text (message ^ "\n")
  else View.send status markup (html "Error" ("<p>" ^ View.escape message ^ "</p>"))

let contest token =
  let root = path (data ()) "contests" in
  match Bj.Contest.find root token with
  | None -> fail "404 Not Found" "Not found"
  | Some c -> c

let open_now c =
  let now = int_of_float (Unix.gettimeofday ()) in
  now >= c.Bj.Contest.start && now < c.stop

let started c = int_of_float (Unix.gettimeofday ()) >= c.Bj.Contest.start

let date timestamp =
  let t = Unix.gmtime (float timestamp) in
  Printf.sprintf "%04d-%02d-%02d %02d:%02d UTC" (t.tm_year + 1900) (t.tm_mon + 1) t.tm_mday t.tm_hour t.tm_min

let problem c name =
  match List.find_opt (fun p -> p.Bj.Contest.name = name) c.Bj.Contest.problems with
  | Some p -> p
  | None -> fail "404 Not Found" "No such problem"

let navigation c =
  let b = base c.Bj.Contest.token in
  "<nav>" ^ View.link b "Problems" ^ " | " ^ View.link (b ^ "/submit") "Submit" ^
  " | " ^ View.link (b ^ "/submissions") "My submissions" ^
  " | " ^ View.link (b ^ "/scoreboard") "Scoreboard" ^
  " | " ^ View.link (b ^ "/submit.sh") "Command-line submitter" ^ "</nav>"

let receive_name () =
  match View.name_cookie () with
  | Some name when Bj.Sub.name_ok name -> name
  | _ -> ""

let submit_form c selected =
  let b = base c.Bj.Contest.token in
  let options = List.map (fun p ->
    let name = p.Bj.Contest.name in
    "<option value=\"" ^ View.escape name ^ "\"" ^ (if name = selected then " selected" else "") ^
    ">" ^ View.escape (name ^ ": " ^ p.title) ^ "</option>") c.problems in
  "<form method=\"post\" enctype=\"multipart/form-data\" action=\"" ^ View.escape (b ^ "/submit") ^ "\">" ^
  "<p><label>Name <input name=\"name\" required maxlength=\"32\" value=\"" ^ View.escape (receive_name ()) ^ "\"></label></p>" ^
  "<p><label>Problem <select name=\"problem\">" ^ String.concat "" options ^ "</select></label></p>" ^
  "<p><label>Language <select name=\"lang\">" ^
  "<option value=\"c\">C</option><option value=\"cpp\">C++</option><option value=\"py\">Python</option>" ^
  "<option value=\"rs\">Rust</option><option value=\"ml\">OCaml</option></select></label></p>" ^
  "<p><label>Paste code<br><textarea name=\"source\" rows=\"18\" cols=\"80\" spellcheck=\"false\"></textarea></label></p>" ^
  "<p><label>Or upload a file <input type=\"file\" name=\"file\"></label></p>" ^
  "<p><button type=\"submit\">Submit</button></p></form>"

let read_file file =
  let ic = open_in_bin file in
  let result = really_input_string ic (in_channel_length ic) in
  close_in ic;
  result

let id_ok id =
  String.length id = 30 && id.[13] = '-' &&
  let rec check i =
    if i = 30 then true
    else if i = 13 then check (i + 1)
    else let c = id.[i] in
      if (i < 13 && c >= '0' && c <= '9') ||
         (i > 13 && ((c >= '0' && c <= '9') || (c >= 'a' && c <= 'f'))) then check (i + 1) else false
  in
  check 0

let submissions c =
  let items dir =
    let folder = path (data ()) dir in
    let names = List.filter id_ok (Array.to_list (Sys.readdir folder)) in
    List.filter_map (fun id ->
      try let s = Bj.Sub.read (path folder id) in
        if s.contest = Filename.basename c.Bj.Contest.dir then Some s else None
      with _ -> None) names in
  List.sort (fun a b -> String.compare b.Bj.Sub.id a.id) (List.append (items "pending") (items "done"))

let submission c id =
  if not (id_ok id) then fail "404 Not Found" "Not found";
  let done_file = path (path (data ()) "done") id in
  let pending_file = path (path (data ()) "pending") id in
  let file = if Sys.file_exists done_file then done_file else pending_file in
  let s = try Bj.Sub.read file with _ -> fail "404 Not Found" "Not found" in
  if s.contest <> Filename.basename c.Bj.Contest.dir then fail "404 Not Found" "Not found";
  let verdict = match s.result with
    | None -> "pending"
    | Some r -> Printf.sprintf "%s cpu=%d test=%d" r.verdict r.cpu r.test in
  let ce = if s.result <> None && Sys.file_exists (done_file ^ ".ce") then read_file (done_file ^ ".ce") else "" in
  (s, verdict, ce)

let scoreboard c =
  let subs = submissions c in
  let people = Hashtbl.create 32 in
  List.iter (fun s ->
    let solved = try Hashtbl.find people s.Bj.Sub.name with Not_found -> [] in
    let solved = match s.result with
      | Some r when r.verdict = "AC" && not (List.mem s.problem solved) -> s.problem :: solved
      | _ -> solved in
    Hashtbl.replace people s.name solved) subs;
  let rows = Hashtbl.fold (fun name solved acc -> (name, List.length solved, solved) :: acc) people [] in
  let rows = List.sort (fun (a, x, _) (b, y, _) ->
    let n = compare y x in if n = 0 then String.compare a b else n) rows in
  let rec render pos last rank = function
    | [] -> ""
    | (name, n, solved) :: rest ->
        let rank = if n = last then rank else pos in
        "<tr><td>" ^ string_of_int rank ^ "</td><td>" ^ View.escape name ^ "</td><td>" ^ string_of_int n ^
        "</td><td>" ^ View.escape (String.concat ", " (List.sort compare solved)) ^ "</td></tr>" ^
        render (pos + 1) n rank rest
  in
  "<table><thead><tr><th>Rank</th><th>Name</th><th>Solved</th><th>Problems</th></tr></thead><tbody>" ^
  render 1 (-1) 0 rows ^ "</tbody></table>"

let script c =
  let host = env "HTTP_X_FORWARDED_HOST" in
  let host = if host = "" then env "HTTP_HOST" else host in
  if host = "" || not (String.for_all (function 'a'..'z' | 'A'..'Z' | '0'..'9' | '.' | '-' | ':' -> true | _ -> false) host) then
    fail "400 Bad Request" "Invalid host";
  let scheme = if env "HTTP_X_FORWARDED_PROTO" = "https" then "https" else "http" in
  let url = scheme ^ "://" ^ host ^ base c.Bj.Contest.token in
  "#!/bin/sh\nset -eu\n" ^
  "URL='" ^ url ^ "'\n" ^
  "NAME=${JUDGE_NAME:?set JUDGE_NAME to your name}\n" ^
  "[ $# -eq 2 ] || { echo 'usage: submit PROBLEM FILE' >&2; exit 2; }\n" ^
  "case $2 in\n" ^
  "  *.c) lang=c ;;\n  *.cpp|*.cc) lang=cpp ;;\n  *.py) lang=py ;;\n  *.rs) lang=rs ;;\n  *.ml) lang=ml ;;\n" ^
  "  *) echo 'unknown file extension' >&2; exit 2 ;;\nesac\n" ^
  "id=$(curl -fsS -H 'Accept: text/plain' --form-string \"name=$NAME\" --form-string \"problem=$1\" --form-string \"lang=$lang\" -F \"file=@$2\" \"$URL/submit\")\n" ^
  "echo \"submitted $id\"\n" ^
  "while :; do\n  result=$(curl -fsS -H 'Accept: text/plain' \"$URL/s/$id\")\n" ^
  "  case $result in\n    pending) sleep 2 ;;\n    AC*) printf '%s\\n' \"$result\"; exit 0 ;;\n" ^
  "    *) printf '%s\\n' \"$result\"; exit 1 ;;\n  esac\ndone\n"

let post c =
  if not (open_now c) then fail "403 Forbidden" "Contest is not accepting submissions";
  let fields = try Bj.Form.read (env "CONTENT_TYPE") (env "CONTENT_LENGTH") with
    | Invalid_argument message -> fail "400 Bad Request" message
    | End_of_file -> fail "400 Bad Request" "Truncated submission" in
  let name = Bj.Form.get fields "name" in
  let p = segment (Bj.Form.get fields "problem") in
  ignore (problem c p);
  let lang = try Bj.Lang.of_string (Bj.Form.get fields "lang") with _ -> fail "400 Bad Request" "Unknown language" in
  if not (Bj.Sub.name_ok name) then fail "400 Bad Request" "Invalid name";
  let file = match List.assoc_opt "file" fields with Some s -> s | None -> "" in
  let paste = match List.assoc_opt "source" fields with Some s -> s | None -> "" in
  let source = if file <> "" then file else paste in
  if source = "" || String.length source > 65536 then fail "400 Bad Request" "Source must be between 1 and 65536 bytes";
  let id = Bj.Sub.id () in
  let s : Bj.Sub.t = { id; contest = Filename.basename c.Bj.Contest.dir; problem = p; name; lang;
    at = int_of_float (Unix.gettimeofday () *. 1000.); attempts = 0; result = None; source } in
  let dir = path (data ()) "pending" in
  Bj.Sub.write (path dir ("." ^ id)) s;
  Sys.rename (path dir ("." ^ id)) (path dir id);
  if plain () then View.send "201 Created" text (id ^ "\n")
  else View.send ~headers:[ "Location", base c.token ^ "/s/" ^ id;
    "Set-Cookie", View.cookie c.token name (env "HTTP_X_FORWARDED_PROTO" = "https") ]
    "303 See Other" markup ""

let get c tail =
  let b = base c.Bj.Contest.token in
  match tail with
  | [] ->
      let body = if not (started c) then "<p>Contest has not started.</p>"
        else let problems = List.map (fun p ->
          "<li>" ^ View.link (b ^ "/p/" ^ p.Bj.Contest.name) (p.name ^ ": " ^ p.title) ^ "</li>") c.problems in
          "<ul>" ^ String.concat "" problems ^ "</ul>" ^
          (if open_now c then "<p>Submissions are open.</p>" else "<p>Contest has ended.</p>") in
      View.send "200 OK" markup (html c.title (navigation c ^ "<p>" ^ date c.start ^ " to " ^ date c.stop ^ "</p>" ^ body))
  | [ "p"; name ] when started c ->
      let p = problem c (segment name) in
      let statement = read_file (path (path c.dir p.name) "statement.html") in
      View.send "200 OK" markup (html p.title (navigation c ^ statement ^
        (if open_now c then submit_form c p.name else "")))
  | [ "submit" ] ->
      if not (open_now c) then fail "403 Forbidden" "Contest is not accepting submissions";
      View.send "200 OK" markup (html "Submit" (navigation c ^ submit_form c ""))
  | [ "submit.sh" ] -> View.send "200 OK" "text/x-shellscript; charset=utf-8" (script c)
  | [ "s"; id ] ->
      let s, verdict, ce = submission c id in
      if plain () then View.send "200 OK" text (verdict ^ "\n" ^ ce)
      else View.send ~headers:(if s.result = None then [ "Refresh", "3" ] else []) "200 OK" markup
        (html "Submission" (navigation c ^ "<p>" ^ View.escape (s.name ^ " / " ^ s.problem ^ " / " ^ verdict) ^ "</p>" ^
          (if ce = "" then "" else "<pre>" ^ View.escape ce ^ "</pre>")))
  | [ "submissions" ] ->
      let name = receive_name () in
      let rows = if name = "" then "<p>Enter your name when submitting to see your submissions here.</p>"
        else let subs = List.filter (fun s -> s.Bj.Sub.name = name) (submissions c) in
          "<ul>" ^ String.concat "" (List.map (fun (s : Bj.Sub.t) ->
            let verdict = match s.result with None -> "pending" | Some r -> r.verdict in
            "<li>" ^ View.link (b ^ "/s/" ^ s.id) (s.problem ^ " / " ^ verdict) ^ "</li>") subs) ^ "</ul>" in
      View.send "200 OK" markup (html "My submissions" (navigation c ^ rows))
  | [ "scoreboard" ] -> View.send ~headers:[ "Refresh", "10" ] "200 OK" markup
      (html "Scoreboard" (navigation c ^ scoreboard c))
  | _ -> fail "404 Not Found" "Not found"

let route () =
  let uri = env "REQUEST_URI" in
  let uri = match String.index_opt uri '?' with None -> uri | Some i -> String.sub uri 0 i in
  let parts = String.split_on_char '/' uri in
  match parts with
  | "" :: "c" :: token :: tail when valid token ->
      let c = contest token in
      let tail = if tail = [ "" ] then [] else tail in
      (match env "REQUEST_METHOD", tail with
      | "GET", _ -> get c tail
       | "POST", [ "submit" ] -> post c
       | _ -> fail "405 Method Not Allowed" "Method not allowed")
  | "" :: tail -> Admin.handle (if tail = [ "" ] then [] else tail)
  | _ -> fail "404 Not Found" "Not found"

let () =
  try route () with
  | Admin.Http (status, message) -> error status message
  | Http (status, message) -> error status message
  | Invalid_argument message -> error "400 Bad Request" message
  | ex -> prerr_endline (Printexc.to_string ex); error "500 Internal Server Error" "Internal error"
