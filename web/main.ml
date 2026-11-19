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
let markup = "text/html; charset=utf-8"

let error status message =
  View.send status markup (html "Error" ("<p>" ^ View.escape message ^ "</p>"))

let contest token =
  let root = path (data ()) "contests" in
  match Bj.Contest.find root token with
  | None -> fail "404 Not Found" "Not found"
  | Some c -> c

let problem c name =
  match List.find_opt (fun p -> p.Bj.Contest.name = name) c.Bj.Contest.problems with
  | Some p -> p
  | None -> fail "404 Not Found" "No such problem"

let navigation c =
  let b = base c.Bj.Contest.token in
  let account = match Account.current () with
    | Some user -> View.link "/c/me" user.author
    | None -> View.link ("/c/login?next=" ^ c.token) "Sign in" ^ " | " ^
        View.link ("/c/signup?next=" ^ c.token) "Sign up" in
  "<nav>" ^ View.link b "Problems" ^ " | " ^ View.link (b ^ "/submit") "Submit" ^
  " | " ^ View.link (b ^ "/submissions") "My submissions" ^
  " | " ^ View.link (b ^ "/scoreboard") "Scoreboard" ^
  (if c.phase = Bj.Contest.Stopped || c.phase = Bj.Contest.Hacking || c.phase = Bj.Contest.Finished
    then " | " ^ View.link (b ^ "/solutions") "Solutions" else "") ^
  (if c.phase = Bj.Contest.Hacking then " | " ^ View.link (b ^ "/hack") "Hack" else "") ^
  (if c.phase = Bj.Contest.Hacking || c.phase = Bj.Contest.Finished
    then " | " ^ View.link (b ^ "/hacks") "Hacks" else "") ^ " | " ^ account ^ "</nav>"

let submit_form (c : Bj.Contest.t) selected =
  match Account.current () with
  | None -> "<p>" ^ View.link ("/c/login?next=" ^ c.token) "Sign in" ^
      " or " ^ View.link ("/c/signup?next=" ^ c.token) "create an account" ^ " to submit.</p>"
  | Some user ->
  let b = base c.Bj.Contest.token in
  let options = List.map (fun p ->
    let name = p.Bj.Contest.name in
    "<option value=\"" ^ View.escape name ^ "\"" ^ (if name = selected then " selected" else "") ^
    ">" ^ View.escape (name ^ ": " ^ p.title) ^ "</option>") c.problems in
  "<form method=\"post\" enctype=\"multipart/form-data\" action=\"" ^ View.escape (b ^ "/submit") ^ "\">" ^
  "<p>Submitting as " ^ View.escape user.author ^ ".</p>" ^
  "<input type=\"hidden\" name=\"csrf\" value=\"" ^ user.csrf ^ "\">" ^
  "<input type=\"hidden\" name=\"request\" value=\"" ^ Bj.Sub.id () ^ "\">" ^
  "<p><label>Problem <select name=\"problem\">" ^ String.concat "" options ^ "</select></label></p>" ^
  "<p><label>Language <select name=\"lang\">" ^
  "<option value=\"c\">C</option><option value=\"cpp\">C++</option><option value=\"py\">Python</option>" ^
  "<option value=\"rs\">Rust</option><option value=\"ml\">OCaml</option></select></label></p>" ^
  "<p><label>Paste code<br><textarea name=\"source\" rows=\"18\" cols=\"80\" maxlength=\"65536\" spellcheck=\"false\"></textarea></label></p>" ^
  "<p><label>Or upload a file <input type=\"file\" name=\"file\"></label></p>" ^
  "<p><button type=\"submit\">Submit</button></p></form>"

let submissions c =
  let items dir =
    let folder = path (data ()) dir in
    let names = List.filter Bj.Sub.id_ok (Array.to_list (Sys.readdir folder)) in
    List.filter_map (fun id ->
      try let s = Bj.Sub.read (path folder id) in
        if s.contest = Filename.basename c.Bj.Contest.dir then Some s else None
      with _ -> None) names in
  List.sort (fun a b -> String.compare b.Bj.Sub.id a.id) (List.append (items "pending") (items "done"))

let submission c id =
  if not (Bj.Sub.id_ok id) then fail "404 Not Found" "Not found";
  let done_file = path (path (data ()) "done") id in
  let pending_file = path (path (data ()) "pending") id in
  let file = if Sys.file_exists done_file then done_file else pending_file in
  let s = try Bj.Sub.read file with _ -> fail "404 Not Found" "Not found" in
  if s.contest <> Filename.basename c.Bj.Contest.dir then fail "404 Not Found" "Not found";
  let verdict = match s.result with
    | None -> "pending"
    | Some r -> Printf.sprintf "%s cpu=%d test=%d" r.verdict r.cpu r.test in
  let ce = if s.result <> None && Sys.file_exists (done_file ^ ".ce") then Bj.File.read (done_file ^ ".ce") else "" in
  (s, verdict, ce)

let scoreboard c =
  let subs = submissions c in
  let people = Hashtbl.create 32 in
  List.iter (fun s ->
    let solved, hacked = try Hashtbl.find people s.Bj.Sub.author with Not_found -> ([], []) in
    let solved, hacked = match s.result with
      | Some r when r.verdict = "AC" -> (s.problem :: solved, hacked)
      | Some r when r.verdict = "HACKED" -> (solved, s.problem :: hacked)
      | _ -> (solved, hacked) in
    Hashtbl.replace people s.author (solved, hacked)) subs;
  let rows = Hashtbl.fold (fun name (solved, hacked) acc ->
    let solved = List.sort_uniq compare solved in
    let hacked = List.sort_uniq compare hacked in
    (name, List.length solved, solved, hacked) :: acc) people [] in
  let rows = List.sort (fun (a, x, _, _) (b, y, _, _) ->
    let n = compare y x in if n = 0 then String.compare a b else n) rows in
  let rec render pos last rank = function
    | [] -> ""
    | (name, n, solved, hacked) :: rest ->
        let rank = if n = last then rank else pos in
        "<tr><td>" ^ string_of_int rank ^ "</td><td>" ^ View.escape name ^ "</td><td>" ^ string_of_int n ^
        "</td><td>" ^ View.escape (String.concat ", " solved) ^ "</td><td>" ^
        View.escape (String.concat ", " hacked) ^ "</td></tr>" ^
        render (pos + 1) n rank rest
  in
  "<table><thead><tr><th>Rank</th><th>Name</th><th>Solved</th><th>Problems</th><th>Hacked</th></tr></thead><tbody>" ^
  render 1 (-1) 0 rows ^ "</tbody></table>"

let hacks c =
  let entries status =
    let dir = path (path (data ()) "hacks") status in
    List.filter_map (fun id ->
      if not (Bj.Sub.id_ok id) then None else try
        let h = Bj.Hack.read (path dir id) in
        if h.contest = Filename.basename c.Bj.Contest.dir then Some h else None
      with _ -> None) (Array.to_list (Sys.readdir dir)) in
  List.sort (fun a b -> String.compare b.Bj.Hack.id a.id)
    (List.append (entries "pending") (entries "done"))

let hack_result h =
  if h.Bj.Hack.result = Some Bj.Hack.Success &&
     not (Sys.file_exists (path (path (path (data ()) "hacks") "done") (h.id ^ ".applied")))
  then None else h.result

let require_user () = match Account.current () with
  | Some user -> user
  | None -> fail "401 Unauthorized" "Sign in first"

let hack_form (c : Bj.Contest.t) =
  match Account.current () with
  | None -> "<p>" ^ View.link ("/c/login?next=" ^ c.Bj.Contest.token) "Sign in" ^
      " or " ^ View.link ("/c/signup?next=" ^ c.token) "create an account" ^ " to hack.</p>"
  | Some user ->
  let b = base c.Bj.Contest.token in
  let options = List.filter_map (fun (s : Bj.Sub.t) -> match s.result with
    | Some r when r.verdict = "AC" && s.author <> user.author ->
        Some ("<option value=\"" ^ s.id ^ "\">" ^ View.escape (s.author ^ " / " ^ s.problem) ^ "</option>")
    | _ -> None) (submissions c) in
  if options = [] then "<p>No accepted solutions to target yet.</p>" else
  "<form method=\"post\" enctype=\"multipart/form-data\" action=\"" ^ b ^ "/hack\">" ^
  "<p>Hacking as " ^ View.escape user.author ^ ".</p>" ^
  "<input type=\"hidden\" name=\"csrf\" value=\"" ^ user.csrf ^ "\">" ^
  "<input type=\"hidden\" name=\"request\" value=\"" ^ Bj.Sub.id () ^ "\">" ^
  "<p><label>Target <select name=\"target\">" ^ String.concat "" options ^ "</select></label></p>" ^
  "<p><label>Proposed input<br><textarea name=\"input\" rows=\"12\" cols=\"80\" maxlength=\"65536\"></textarea></label></p>" ^
  "<p><label>Or upload input <input name=\"file\" type=\"file\"></label></p>" ^
  "<button>Submit hack</button></form>"

let post_hack ?fields (c : Bj.Contest.t) =
  let user = require_user () in
  let fields = match fields with Some fields -> fields | None -> Bj.Form.read (env "CONTENT_TYPE") (env "CONTENT_LENGTH") in
  if not (Bj.Contest.same (Bj.Form.get fields "csrf") user.csrf) then
    fail "403 Forbidden" "Invalid form token";
  let target = Bj.Form.get fields "target" in
  if not (Bj.Sub.id_ok target) then fail "400 Bad Request" "Invalid target";
  let file = match List.assoc_opt "file" fields with Some s -> s | None -> "" in
  let pasted = match List.assoc_opt "input" fields with Some s -> s | None -> "" in
  let input = if file <> "" then file else pasted in
  if input = "" || String.length input > 65536 then fail "400 Bad Request" "Input must be 1 to 65536 bytes";
  let id = match !(Peer.request) with
    | None -> Bj.Sub.id ()
    | Some id -> if Bj.Form.get fields "request" <> id then fail "400 Bad Request" "Invalid request ID"; id in
  Peer.with_request_lock (fun () -> Bj.Contest.with_lock c.dir (fun () ->
    let pending = path (path (path (data ()) "hacks") "pending") id in
    let done_file = path (path (path (data ()) "hacks") "done") id in
    if Sys.file_exists pending || Sys.file_exists done_file then begin
      let h = Bj.Hack.read (if Sys.file_exists done_file then done_file else pending) in
      if h.author <> user.author || h.contest <> Filename.basename c.dir ||
         h.target <> target || h.input <> input then fail "409 Conflict" "Request ID already used"
    end else begin
      if (Bj.Contest.load c.dir).phase <> Bj.Contest.Hacking then fail "403 Forbidden" "Hacks are not open";
      let target_file = path (path (data ()) "done") target in
      let s = try Bj.Sub.read target_file with _ -> fail "404 Not Found" "Target not found" in
      if s.contest <> Filename.basename c.dir || s.author = user.author ||
         (match s.result with Some r -> r.verdict <> "AC" | None -> true) then
        fail "400 Bad Request" "Target must be another contestant's accepted submission";
      let h : Bj.Hack.t = { id; contest = s.contest; problem = s.problem; target; author = user.author;
        at = int_of_float (Unix.gettimeofday () *. 1000.); attempts = 0; result = None; input; expected = None } in
      Bj.Hack.write pending h
    end));
  View.send ~headers:[ "Location", base c.token ^ "/h/" ^ id ]
    "303 See Other" markup ""

let post ?fields (c : Bj.Contest.t) =
  let user = require_user () in
  let fields = match fields with Some fields -> fields | None -> try Bj.Form.read (env "CONTENT_TYPE") (env "CONTENT_LENGTH") with
    | Invalid_argument message -> fail "400 Bad Request" message
    | End_of_file -> fail "400 Bad Request" "Truncated submission" in
  if not (Bj.Contest.same (Bj.Form.get fields "csrf") user.csrf) then
    fail "403 Forbidden" "Invalid form token";
  let p = segment (Bj.Form.get fields "problem") in
  ignore (problem c p);
  let lang = try Bj.Lang.of_string (Bj.Form.get fields "lang") with _ -> fail "400 Bad Request" "Unknown language" in
  let file = match List.assoc_opt "file" fields with Some s -> s | None -> "" in
  let paste = match List.assoc_opt "source" fields with Some s -> s | None -> "" in
  let source = if file <> "" then file else paste in
  if source = "" || String.length source > 65536 then fail "400 Bad Request" "Source must be between 1 and 65536 bytes";
  let id = match !(Peer.request) with
    | None -> Bj.Sub.id ()
    | Some id -> if Bj.Form.get fields "request" <> id then fail "400 Bad Request" "Invalid request ID"; id in
  let s : Bj.Sub.t = { id; contest = Filename.basename c.Bj.Contest.dir; problem = p; author = user.author; lang;
    at = int_of_float (Unix.gettimeofday () *. 1000.); attempts = 0; result = None; source } in
  let dir = path (data ()) "pending" in
  Peer.with_request_lock (fun () -> Bj.Contest.with_lock c.dir (fun () ->
    let pending = path dir id and done_file = path (path (data ()) "done") id in
    if Sys.file_exists pending || Sys.file_exists done_file then begin
      let old = Bj.Sub.read (if Sys.file_exists done_file then done_file else pending) in
      if old.author <> s.author || old.contest <> s.contest || old.problem <> s.problem ||
         old.lang <> s.lang || old.source <> s.source then fail "409 Conflict" "Request ID already used"
    end else begin
      if (Bj.Contest.load c.dir).phase <> Bj.Contest.Running then
        fail "403 Forbidden" "Contest is not accepting submissions";
      Bj.Sub.write pending s
    end));
  View.send ~headers:[ "Location", base c.token ^ "/s/" ^ id ]
    "303 See Other" markup ""

let get c tail =
  let b = base c.Bj.Contest.token in
  match tail with
  | [] ->
      let body = if c.phase = Bj.Contest.Ready then "<p>Contest has not started.</p>"
        else let problems = List.map (fun p ->
          "<li>" ^ View.link (b ^ "/p/" ^ p.Bj.Contest.name) (p.name ^ ": " ^ p.title) ^ "</li>") c.problems in
          "<ul>" ^ String.concat "" problems ^ "</ul>" ^
          (match c.phase with
          | Bj.Contest.Running -> "<p>Submissions are open.</p>"
          | Bj.Contest.Stopped -> "<p>Submissions have stopped. Hacks have not started.</p>"
          | Bj.Contest.Hacking -> "<p>Hacks are open.</p>"
          | Bj.Contest.Finished -> "<p>Contest is finished.</p>"
          | Bj.Contest.Ready -> "") in
      View.send "200 OK" markup (html c.title (navigation c ^ body))
  | [ "p"; name ] when c.phase <> Bj.Contest.Ready ->
      let p = problem c (segment name) in
      let statement = Bj.File.read (path (path c.dir p.name) "statement.html") in
      View.send "200 OK" markup (html p.title (navigation c ^ statement ^
        (if c.phase = Bj.Contest.Running then submit_form c p.name else "")))
  | [ "submit" ] ->
      if c.phase <> Bj.Contest.Running then fail "403 Forbidden" "Contest is not accepting submissions";
      View.send "200 OK" markup (html "Submit" (navigation c ^ submit_form c ""))
  | [ "s"; id ] ->
      let s, verdict, ce = submission c id in
      let reveal = c.phase <> Bj.Contest.Running && c.phase <> Bj.Contest.Ready &&
        (match s.result with Some r -> r.verdict = "AC" || r.verdict = "HACKED" | None -> false) in
      View.send ~headers:(if s.result = None then [ "Refresh", "3" ] else []) "200 OK" markup
        (html "Submission" (navigation c ^ "<p>" ^ View.escape (s.author ^ " / " ^ s.problem ^ " / " ^ verdict) ^ "</p>" ^
          (if ce = "" then "" else "<pre>" ^ View.escape ce ^ "</pre>") ^
          (if reveal then "<h2>Source</h2><pre>" ^ View.escape s.source ^ "</pre>" else "")))
  | [ "solutions" ] when c.phase <> Bj.Contest.Ready && c.phase <> Bj.Contest.Running ->
      let rows = List.filter_map (fun (s : Bj.Sub.t) -> match s.result with
        | Some r when r.verdict = "AC" || r.verdict = "HACKED" ->
            Some ("<li>" ^ View.link (b ^ "/s/" ^ s.id) (s.author ^ " / " ^ s.problem ^ " / " ^ r.verdict) ^ "</li>")
        | _ -> None) (submissions c) in
      View.send "200 OK" markup (html "Solutions" (navigation c ^ "<ul>" ^ String.concat "" rows ^ "</ul>"))
  | [ "hack" ] ->
      if c.phase <> Bj.Contest.Hacking then fail "403 Forbidden" "Hacks are not open";
      View.send "200 OK" markup (html "Propose a hack" (navigation c ^ hack_form c))
  | [ "hacks" ] when c.phase = Bj.Contest.Hacking || c.phase = Bj.Contest.Finished ->
      let rows = List.map (fun (h : Bj.Hack.t) ->
        let status = match hack_result h with None -> "pending" | Some r -> Bj.Hack.string_of_outcome r in
        "<li>" ^ View.link (b ^ "/h/" ^ h.id) (h.author ^ " / " ^ status) ^ "</li>") (hacks c) in
      View.send "200 OK" markup (html "Hacks" (navigation c ^ "<ul>" ^ String.concat "" rows ^ "</ul>"))
  | [ "h"; id ] when c.phase = Bj.Contest.Hacking || c.phase = Bj.Contest.Finished ->
      if not (Bj.Sub.id_ok id) then fail "404 Not Found" "Hack not found";
      let dir = path (data ()) "hacks" in
      let finished = path (path dir "done") id in
      let file = if Sys.file_exists finished then finished else path (path dir "pending") id in
      let h = try Bj.Hack.read file with _ -> fail "404 Not Found" "Hack not found" in
      if h.contest <> Filename.basename c.dir then fail "404 Not Found" "Hack not found";
      let result = hack_result h in
      let status = match result with None -> "pending" | Some r -> Bj.Hack.string_of_outcome r in
      View.send ~headers:(if result = None then [ "Refresh", "3" ] else []) "200 OK" markup
        (html "Hack" (navigation c ^ "<p>" ^ View.escape (h.author ^ " / " ^ status) ^
          " against " ^ View.link (b ^ "/s/" ^ h.target) h.target ^ "</p>" ^
          "<h2>Proposed input</h2><pre>" ^ View.escape h.input ^ "</pre>"))
  | [ "submissions" ] ->
      let rows = match Account.current () with
        | None -> "<p>" ^ View.link ("/c/login?next=" ^ c.token) "Sign in" ^
            " to see your submissions.</p>"
        | Some user -> let subs = List.filter (fun s -> s.Bj.Sub.author = user.author) (submissions c) in
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
  | [ ""; "c"; "theme.css" ] when env "REQUEST_METHOD" = "GET" ->
      View.send "200 OK" "text/css; charset=utf-8" (Bj.File.read "/src/web/theme.css")
  | [ ""; "c"; "instance-key" ] when env "REQUEST_METHOD" = "GET" -> Peer.public_key ()
  | [ ""; "c"; "join" ] -> Peer.handle_join ()
  | [ ""; "c"; page ] when List.mem page [ "signup"; "login"; "logout"; "me" ] -> Account.handle page
  | "" :: "c" :: "f" :: "v1" :: token :: tail when Account.next_token token = token && token <> "" ->
      let c = contest token in
      let user, request, body = Peer.incoming () in
      Account.remote := Some user;
      Peer.request := (if env "REQUEST_METHOD" = "POST" then Some request else None);
      (match env "REQUEST_METHOD", tail with
       | "GET", _ -> get c tail
       | "POST", [ "submit" ] -> post ~fields:(Bj.Form.parse (env "CONTENT_TYPE") body) c
       | "POST", [ "hack" ] -> post_hack ~fields:(Bj.Form.parse (env "CONTENT_TYPE") body) c
       | _ -> fail "405 Method Not Allowed" "Method not allowed")
  | "" :: "c" :: token :: tail when valid token ->
      let tail = if tail = [ "" ] then [] else tail in
      (match Bj.Contest.find (path (data ()) "contests") token with
       | Some c ->
           (match env "REQUEST_METHOD", tail with
            | "GET", _ -> get c tail
            | "POST", [ "submit" ] -> post c
            | "POST", [ "hack" ] -> post_hack c
            | _ -> fail "405 Method Not Allowed" "Method not allowed")
       | None when Account.next_token token = token && Peer.remote token <> None -> Peer.forward token tail
       | None -> fail "404 Not Found" "Not found")
  | "" :: tail -> Admin.handle (if tail = [ "" ] then [] else tail)
  | _ -> fail "404 Not Found" "Not found"

let () =
  try route () with
  | Admin.Http (status, message) -> error status message
  | Http (status, message) -> error status message
  | Invalid_argument message -> error "400 Bad Request" message
  | ex -> prerr_endline (Printexc.to_string ex); error "500 Internal Server Error" "Internal error"
