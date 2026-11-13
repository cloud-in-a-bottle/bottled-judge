exception Http of string * string

let fail status message = raise (Http (status, message))
let bad message = fail "400 Bad Request" message
let env key = try Sys.getenv key with Not_found -> ""
let data () = Sys.getenv "OPENHOST_APP_DATA_DIR"
let path = Filename.concat
let escape = View.escape
let link = View.link

let page ?(headers = []) title body =
  View.send ~headers "200 OK" "text/html; charset=utf-8"
    (View.page title ("<nav>" ^ link "/" "Contests" ^ "</nav>" ^ body))

let redirect target = View.send ~headers:[ "Location", target ] "303 See Other" "text/html; charset=utf-8" ""

let contest_id_ok id =
  String.length id > 0 && String.length id <= 64 &&
  String.for_all (function 'a'..'z' | 'A'..'Z' | '0'..'9' | '-' | '_' -> true | _ -> false) id

let valid_id id = if not (contest_id_ok id) then fail "404 Not Found" "Unknown contest" else id

let letter p =
  if String.length p <> 1 || p.[0] < 'a' || p.[0] > 'z' then fail "404 Not Found" "Unknown problem";
  p

let form ?(limit = 131072) () =
  try Bj.Form.read ~limit (env "CONTENT_TYPE") (env "CONTENT_LENGTH") with
  | Invalid_argument message -> bad message

let get fields key = try Bj.Form.get fields key with Invalid_argument _ -> bad ("Missing " ^ key)

let draft id =
  let folder = Bj.Admin_data.draft (data ()) (valid_id id) in
  if not (Sys.file_exists folder) || not (Bj.Admin_data.directory folder) then
    fail "404 Not Found" "Unknown draft";
  folder

let contest_fields fields =
  let title = get fields "title" in
  let start = get fields "start" in
  let stop = get fields "end" in
  (try Bj.Admin_data.title title; Bj.Admin_data.check_times start stop with
  | Failure message -> bad message
  | _ -> bad "Invalid UTC date");
  (title, start, stop)

let future start =
  if Bj.Admin_data.date start <= int_of_float (Unix.gettimeofday ()) then
    bad "Start must be in the future"

let token () =
  let ic = open_in_bin "/dev/urandom" in
  let bytes = really_input_string ic 16 in
  close_in ic;
  View.hex bytes

let contest_file dir title secret start stop =
  Bj.Kv.write (path dir "contest.txt")
    [ "title", title; "token", secret; "start", start; "end", stop ] ""

let next_time minutes =
  Bj.Contest.format_utc (int_of_float (Unix.gettimeofday ()) + minutes * 60)

let contest_form action title start stop button =
  "<form method=\"post\" enctype=\"multipart/form-data\" action=\"" ^ escape action ^ "\">" ^
  "<p><label>Title <input name=\"title\" required maxlength=\"100\" value=\"" ^ escape title ^ "\"></label></p>" ^
  "<p><label>Start (UTC, YYYY-MM-DD HH:MM) <input name=\"start\" required value=\"" ^ escape start ^ "\"></label></p>" ^
  "<p><label>End (UTC, YYYY-MM-DD HH:MM) <input name=\"end\" required value=\"" ^ escape stop ^ "\"></label></p>" ^
  "<button>" ^ escape button ^ "</button></form>"

let problem_form action title time memory statement button =
  "<form method=\"post\" enctype=\"multipart/form-data\" action=\"" ^ escape action ^ "\">" ^
  "<p><label>Title <input name=\"title\" required maxlength=\"100\" value=\"" ^ escape title ^ "\"></label></p>" ^
  "<p><label>Time limit (ms) <input name=\"time\" type=\"number\" min=\"100\" max=\"10000\" required value=\"" ^ escape time ^ "\"></label></p>" ^
  "<p><label>Memory limit (MB) <input name=\"memory\" type=\"number\" min=\"32\" max=\"1024\" required value=\"" ^ escape memory ^ "\"></label></p>" ^
  "<p><label>Statement (HTML)<br><textarea name=\"statement\" rows=\"16\" cols=\"80\">" ^ escape statement ^ "</textarea></label></p>" ^
  "<button>" ^ escape button ^ "</button></form>"

let problem_fields fields =
  let title = get fields "title" in
  let time = try int_of_string (get fields "time") with _ -> bad "Invalid time limit" in
  let memory = try int_of_string (get fields "memory") with _ -> bad "Invalid memory limit" in
  let statement = get fields "statement" in
  (try Bj.Admin_data.title title with Failure message -> bad message);
  if time < 100 || time > 10000 || memory < 32 || memory > 1024 then bad "Invalid problem limits";
  if String.length statement > 100000 then bad "Statement is too long";
  (title, time, memory, statement)

let save_problem dir title time memory statement =
  Bj.Kv.write (path dir "problem.txt")
    [ "title", title; "time", string_of_int time; "memory", string_of_int memory ] "";
  Bj.Admin_data.write (path dir "statement.html") statement 0o644

let load_problem dir name =
  let folder = path dir (letter name) in
  if not (Sys.file_exists folder) || not (Bj.Admin_data.directory folder) then
    fail "404 Not Found" "Unknown problem";
  folder

let test_numbers dir =
  let files = Array.to_list (Sys.readdir dir) in
  List.sort_uniq compare (List.filter_map (fun name ->
    let suffix = if Filename.check_suffix name ".in" then ".in"
      else if Filename.check_suffix name ".out" then ".out" else "" in
    if suffix = "" then None else
      let number = Filename.chop_suffix name suffix in
      match int_of_string_opt number with
      | Some n when n > 0 && string_of_int n = number -> Some n
      | _ -> None) files)

let test_form action =
  "<form method=\"post\" enctype=\"multipart/form-data\" action=\"" ^ escape action ^ "\">" ^
  "<p><label>Tests ZIP <input name=\"archive\" type=\"file\" accept=\".zip,application/zip\" required></label></p>" ^
  "<button>Replace all tests</button></form>"

let post_button action label =
  "<form method=\"post\" action=\"" ^ escape action ^ "\"><button>" ^ escape label ^ "</button></form>"

let list_contests () =
  let rows dir status =
    let ids = List.filter contest_id_ok (Array.to_list (Sys.readdir dir)) in
    List.filter_map (fun id ->
      try
        let c = Bj.Contest.load (path dir id) in
        Some ("<li>" ^ link ("/" ^ id) c.title ^ " (" ^ status ^ ", " ^
          string_of_int (List.length c.problems) ^ " problems)</li>")
      with _ -> None) ids in
  page "Contests" ("<p>" ^ link "/new" "Create contest" ^ "</p>" ^
    "<ul>" ^ String.concat "" (List.append
      (rows (Bj.Admin_data.drafts (data ())) "draft")
      (rows (Bj.Admin_data.contests (data ())) "published")) ^ "</ul>")

let new_contest () =
  let title, start, stop = contest_fields (form ()) in
  future start;
  let id = Bj.Sub.id () in
  let dir = Bj.Admin_data.draft (data ()) id in
  Unix.mkdir dir 0o700;
  contest_file dir title (token ()) start stop;
  redirect ("/" ^ id)

let update_contest id =
  let dir = draft id in
  let title, start, stop = contest_fields (form ()) in
  future start;
  let c = Bj.Contest.load dir in
  contest_file dir title c.token start stop;
  redirect ("/" ^ id)

let draft_page id dir =
  let c = Bj.Contest.load dir in
  let base = "/" ^ id in
  let problems = List.map (fun p ->
    "<li>" ^ link (base ^ "/p/" ^ p.Bj.Contest.name) p.title ^ "</li>") c.problems in
  page c.title
    ("<p>Draft. Only you can see it.</p>" ^
    contest_form base c.title
      (Bj.Kv.get (fst (Bj.Kv.read (path dir "contest.txt"))) "start")
      (Bj.Kv.get (fst (Bj.Kv.read (path dir "contest.txt"))) "end") "Save" ^
    "<h2>Problems</h2><ol>" ^ String.concat "" problems ^ "</ol>" ^
    problem_form (base ^ "/p") "" "2000" "256" "" "Add problem" ^
    post_button (base ^ "/publish") "Publish" ^ post_button (base ^ "/delete") "Delete draft")

let draft_problem id name =
  let dir = draft id in
  let folder = load_problem dir name in
  let h, _ = Bj.Kv.read (path folder "problem.txt") in
  let statement = Bj.File.read (path folder "statement.html") in
  let base = "/" ^ id ^ "/p/" ^ name in
  let tests = path folder "tests" in
  let count = List.length (test_numbers tests) in
  page (Bj.Kv.get h "title") ("<p>" ^ link ("/" ^ id) "Back to draft" ^ "</p>" ^
    problem_form base (Bj.Kv.get h "title") (Bj.Kv.get h "time") (Bj.Kv.get h "memory") statement "Save" ^
    "<h2>Preview</h2>" ^ statement ^ "<h2>Hidden tests</h2><p>" ^
    string_of_int count ^ (if count = 1 then " test pair" else " test pairs") ^ " uploaded.</p>" ^
    "<p>ZIP this exact directory layout, with no surrounding folder:</p>" ^
    "<pre>tests/\n├── 1.in\n├── 1.out\n├── 2.in\n└── 2.out</pre>" ^
    "<p>Tests must be consecutive .in/.out pairs. Uploading a ZIP replaces the entire draft test set. " ^
    "Maximum ZIP size: 4 MiB; each file: 2 MiB.</p>" ^
    test_form (base ^ "/tests") ^ post_button (base ^ "/delete") "Delete problem")

let add_problem id =
  let dir = draft id in
  let title, time, memory, statement = problem_fields (form ()) in
  let rec choose letter =
    if letter > Char.code 'z' then bad "Maximum 26 problems";
    let name = String.make 1 (Char.chr letter) in
    if Sys.file_exists (path dir name) then choose (letter + 1) else name in
  let name = choose (Char.code 'a') in
  let staging = path dir (".new-" ^ Bj.Sub.id ()) in
  Unix.mkdir staging 0o700;
  Unix.mkdir (path staging "tests") 0o700;
  save_problem staging title time memory statement;
  Sys.rename staging (path dir name);
  redirect ("/" ^ id ^ "/p/" ^ name)

let edit_problem id name =
  let dir = draft id in
  let folder = load_problem dir name in
  let title, time, memory, statement = problem_fields (form ()) in
  save_problem folder title time memory statement;
  redirect ("/" ^ id ^ "/p/" ^ name)

let replace_tests id name =
  let dir = draft id in
  let folder = load_problem dir name in
  let fields = form ~limit:4194304 () in
  (try Test_archive.replace folder (get fields "archive") with
  | Failure message -> bad message
  | Zip.Error (_, _, message) -> bad ("Invalid ZIP: " ^ message)
  | End_of_file -> bad "Invalid ZIP");
  redirect ("/" ^ id ^ "/p/" ^ name)

let delete_problem id name =
  let dir = draft id in
  Bj.Admin_data.remove (load_problem dir name);
  redirect ("/" ^ id)

let publish id =
  let dir = draft id in
  (try ignore (Bj.Admin_data.validate (data ()) id) with
  | Failure message -> bad message
  | ex -> bad (Printexc.to_string ex));
  Array.iter (fun name -> if name.[0] = '.' then Bj.Admin_data.remove (path dir name)) (Sys.readdir dir);
  Sys.rename dir (Bj.Admin_data.published (data ()) id);
  redirect ("/" ^ id)

let delete_draft id =
  Bj.Admin_data.remove (draft id);
  redirect "/"

let published_page id dir =
  let c = Bj.Contest.load dir in
  let entries kind =
    let folder = path (data ()) kind in
    List.filter_map (fun file ->
      if not (Bj.Sub.id_ok file) then None else try
        let s = Bj.Sub.read (path folder file) in
        if s.contest = id then Some s else None
      with _ -> None) (Array.to_list (Sys.readdir folder)) in
  let all = List.sort (fun a b -> String.compare b.Bj.Sub.id a.id)
    (List.append (entries "pending") (entries "done")) in
  let recent = List.filteri (fun i _ -> i < 50) all in
  let rows = List.map (fun (s : Bj.Sub.t) ->
    let status = match s.result with None -> "pending" | Some r -> r.verdict in
    "<tr><td>" ^ link ("/" ^ id ^ "/s/" ^ s.id) s.id ^ "</td><td>" ^ escape s.name ^
    "</td><td>" ^ escape s.problem ^ "</td><td>" ^ escape status ^ "</td></tr>") recent in
  let problems = List.map (fun p -> "<li>" ^ escape (p.Bj.Contest.name ^ ": " ^ p.title) ^ "</li>") c.problems in
  page c.title ("<p>Contest link: " ^ link ("/c/" ^ c.token) ("/c/" ^ c.token) ^ "</p>" ^
    "<p>" ^ Bj.Contest.format_utc c.start ^ " UTC to " ^ Bj.Contest.format_utc c.stop ^ " UTC</p>" ^
    "<p>" ^ string_of_int (List.length all) ^ " submissions; " ^
    string_of_int (List.length (List.filter (fun s -> s.Bj.Sub.result = None) all)) ^ " pending.</p>" ^
    "<h2>Problems</h2><ol>" ^ String.concat "" problems ^ "</ol><h2>Recent submissions</h2>" ^
    "<table><tr><th>ID</th><th>Name</th><th>Problem</th><th>Verdict</th></tr>" ^
    String.concat "" rows ^ "</table>")

let view_submission id sid =
  if not (Bj.Sub.id_ok sid) then fail "404 Not Found" "Unknown submission";
  let folders = [ path (data ()) "done"; path (data ()) "pending" ] in
  let file = match List.find_opt (fun dir -> Sys.file_exists (path dir sid)) folders with
    | Some dir -> path dir sid
    | None -> fail "404 Not Found" "Unknown submission" in
  let s = Bj.Sub.read file in
  if s.contest <> id then fail "404 Not Found" "Unknown submission";
  let result = match s.result with None -> "pending"
    | Some r -> Printf.sprintf "%s test=%d cpu=%d" r.verdict r.test r.cpu in
  let ce = path (path (data ()) "done") (sid ^ ".ce") in
  let error = if Sys.file_exists ce then
    "<h2>Compiler output</h2><pre>" ^ escape (Bj.File.read ce) ^ "</pre>" else "" in
  page "Submission" ("<p>" ^ link ("/" ^ id) "Back to contest" ^ "</p>" ^
    "<p>" ^ escape (s.name ^ " / " ^ s.problem ^ " / " ^ result) ^ "</p>" ^
    "<h2>Source</h2><pre>" ^ escape s.source ^ "</pre>" ^ error)

let view_contest id =
  let id = valid_id id in
  let draft = Bj.Admin_data.draft (data ()) id in
  let live = Bj.Admin_data.published (data ()) id in
  if Sys.file_exists draft then draft_page id draft
  else if Sys.file_exists live then published_page id live
  else fail "404 Not Found" "Unknown contest"

let handle parts =
  if env "HTTP_X_OPENHOST_IS_OWNER" <> "true" then fail "403 Forbidden" "Owner access required";
  match env "REQUEST_METHOD", parts with
  | "GET", [] -> list_contests ()
  | "GET", [ "admin" ] -> redirect "/"
  | "GET", [ "new" ] -> page "New contest" (contest_form "/new" "" (next_time 60) (next_time 240) "Create")
  | "POST", [ "new" ] -> new_contest ()
  | "GET", [ id ] -> view_contest id
  | "POST", [ id ] -> update_contest id
  | "POST", [ id; "p" ] -> add_problem id
  | "GET", [ id; "p"; name ] -> draft_problem id (letter name)
  | "POST", [ id; "p"; name ] -> edit_problem id (letter name)
  | "POST", [ id; "p"; name; "tests" ] -> replace_tests id (letter name)
  | "POST", [ id; "p"; name; "delete" ] -> delete_problem id (letter name)
  | "POST", [ id; "publish" ] -> publish id
  | "POST", [ id; "delete" ] -> delete_draft id
  | "GET", [ id; "s"; sid ] -> view_submission (valid_id id) sid
  | _ -> fail "404 Not Found" "Not found"
