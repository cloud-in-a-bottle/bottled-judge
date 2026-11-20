exception Http of string * string

let fail status message = raise (Http (status, message))
let bad message = fail "400 Bad Request" message
let env key = try Sys.getenv key with Not_found -> ""
let data () = Sys.getenv "OPENHOST_APP_DATA_DIR"
let path = Filename.concat
let escape = View.escape
let link = View.link
let contest_path id = "/contests/" ^ id
let manage_path id = contest_path id ^ "/manage"

let admin () = match Account.local () with
  | Some user when user.admin -> user
  | _ -> fail "403 Forbidden" "Admin access required"

let csrf () = "<input type=\"hidden\" name=\"csrf\" value=\"" ^ (admin ()).csrf ^ "\">"

let page ?(headers = []) title body =
  View.send ~headers "200 OK" "text/html; charset=utf-8"
    (View.page title (Account.navigation (Account.local ()) ^ body))

let redirect target = View.send ~headers:[ "Location", target ] "303 See Other" "text/html; charset=utf-8" ""

let contest_id_ok = Bj.Contest.id_ok

let valid_id id = if not (contest_id_ok id) then fail "404 Not Found" "Unknown contest" else id

let letter p =
  if String.length p <> 1 || p.[0] < 'a' || p.[0] > 'z' then fail "404 Not Found" "Unknown problem";
  p

let get fields key = try Bj.Form.get fields key with Invalid_argument _ -> bad ("Missing " ^ key)

let form ?(limit = 131072) () =
  let fields = try Bj.Form.read ~limit (env "CONTENT_TYPE") (env "CONTENT_LENGTH") with
    | Invalid_argument message -> bad message in
  if not (Bj.Contest.same (get fields "csrf") (admin ()).csrf) then
    fail "403 Forbidden" "Invalid form token";
  fields

let draft id =
  let folder = Bj.Admin_data.draft (data ()) (valid_id id) in
  if not (Sys.file_exists folder) || not (Bj.Admin_data.directory folder) then
    fail "404 Not Found" "Unknown draft";
  folder

let contest_fields fields =
  let title = get fields "title" in
  (try Bj.Admin_data.title title with Failure message -> bad message);
  title

let next_id () =
  let dir = Bj.Admin_data.drafts (data ()) in
  Bj.Contest.with_lock dir (fun () ->
    let file = path dir ".next" in
    let id = if Sys.file_exists file then int_of_string (String.trim (Bj.File.read file)) else 1 in
    if id > 999999999999999 then failwith "No contest IDs left";
    Bj.Admin_data.write file (string_of_int (id + 1) ^ "\n") 0o600;
    string_of_int id)

let contest_file dir title =
  Bj.Kv.write (path dir "contest.txt")
    [ "title", title; "phase", "ready" ] ""

let contest_form action title button =
  "<form method=\"post\" enctype=\"multipart/form-data\" action=\"" ^ escape action ^ "\">" ^
  csrf () ^
  "<p><label>Title <input name=\"title\" required maxlength=\"100\" value=\"" ^ escape title ^ "\"></label></p>" ^
  "<button>" ^ escape button ^ "</button></form>"

let problem_form action title time memory statement oracle button =
  "<form method=\"post\" enctype=\"multipart/form-data\" action=\"" ^ escape action ^ "\">" ^
  csrf () ^
  "<p><label>Title <input name=\"title\" required maxlength=\"100\" value=\"" ^ escape title ^ "\"></label></p>" ^
  "<p><label>Time limit (ms) <input name=\"time\" type=\"number\" min=\"100\" max=\"10000\" required value=\"" ^ escape time ^ "\"></label></p>" ^
  "<p><label>Memory limit (MB) <input name=\"memory\" type=\"number\" min=\"32\" max=\"1024\" required value=\"" ^ escape memory ^ "\"></label></p>" ^
  "<p><label>Statement (HTML)<br><textarea name=\"statement\" rows=\"16\" cols=\"80\">" ^ escape statement ^ "</textarea></label></p>" ^
  "<p><label>Oracle (Python 3; read proposed input, exit 2 if invalid, otherwise print the exact answer)<br>" ^
  "<textarea name=\"oracle\" rows=\"14\" cols=\"80\">" ^ escape oracle ^ "</textarea></label></p>" ^
  "<button>" ^ escape button ^ "</button></form>"

let problem_fields fields =
  let title = get fields "title" in
  let time = try int_of_string (get fields "time") with _ -> bad "Invalid time limit" in
  let memory = try int_of_string (get fields "memory") with _ -> bad "Invalid memory limit" in
  let statement = get fields "statement" in
  let oracle = get fields "oracle" in
  (try Bj.Admin_data.title title with Failure message -> bad message);
  if time < 100 || time > 10000 || memory < 32 || memory > 1024 then bad "Invalid problem limits";
  if String.length statement > 100000 then bad "Statement is too long";
  if String.length oracle > 65536 then bad "Oracle is too long";
  (title, time, memory, statement, oracle)

let save_problem dir title time memory statement oracle =
  Bj.Kv.write (path dir "problem.txt")
    [ "title", title; "time", string_of_int time; "memory", string_of_int memory ] "";
  Bj.Admin_data.write (path dir "statement.html") statement 0o644;
  let file = path dir "oracle.py" in
  if oracle <> "" then Bj.Admin_data.write file oracle 0o644
  else if Sys.file_exists file then Unix.unlink file

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
  csrf () ^
  "<p><label>Tests ZIP <input name=\"archive\" type=\"file\" accept=\".zip,application/zip\" required></label></p>" ^
  "<button>Replace all tests</button></form>"

let post_button action label =
  "<form method=\"post\" enctype=\"multipart/form-data\" action=\"" ^ escape action ^ "\">" ^
  csrf () ^ "<button>" ^ escape label ^ "</button></form>"

let list_users () =
  let entries = List.map (fun name ->
    "<li>" ^ link ("/users/" ^ name) (Account.author name) ^ "</li>") (Account.users ()) in
  page "Users" ("<ul>" ^ String.concat "" entries ^ "</ul>")

let user_page name =
  let username = Account.normalize name in
  if not (Account.username_ok username) ||
     not (Sys.file_exists (Account.account_file username)) then
    fail "404 Not Found" "Unknown user";
  let notice = if env "QUERY_STRING" = "reset=1" then
    "<p>Password changed. Existing sessions are invalid.</p>" else "" in
  page (Account.author username) (notice ^
    "<form method=\"post\" enctype=\"multipart/form-data\" action=\"/users/" ^ username ^ "/reset\">" ^ csrf () ^
    "<p><label>New password <input type=\"password\" name=\"password\" required minlength=\"12\" maxlength=\"128\" autocomplete=\"new-password\"></label></p>" ^
    "<button>Reset password</button></form>")

let reset_user name =
  let username = Account.normalize name in
  let fields = form () in
  (try Account.reset username (get fields "password") with Invalid_argument message -> bad message);
  redirect ("/users/" ^ username ^ "?reset=1")

let new_contest () =
  let title = contest_fields (form ()) in
  let id = next_id () in
  let dir = Bj.Admin_data.draft (data ()) id in
  Unix.mkdir dir 0o700;
  contest_file dir title;
  redirect (manage_path id)

let update_contest id =
  let dir = draft id in
  let title = contest_fields (form ()) in
  contest_file dir title;
  redirect (manage_path id)

let draft_page id dir =
  let c = Bj.Contest.load dir in
  let base = manage_path id in
  let problems = List.map (fun p ->
    "<li>" ^ link (base ^ "/p/" ^ p.Bj.Contest.name) p.title ^ "</li>") c.problems in
  page c.title
    ("<p>Draft. Only you can see it.</p>" ^
    contest_form base c.title "Save" ^
    "<h2>Problems</h2><ol>" ^ String.concat "" problems ^ "</ol>" ^
    problem_form (base ^ "/p") "" "2000" "256" "" "" "Add problem" ^
    post_button (base ^ "/publish") "Publish" ^ post_button (base ^ "/delete") "Delete draft")

let draft_problem id name =
  let dir = draft id in
  let folder = load_problem dir name in
  let h, _ = Bj.Kv.read (path folder "problem.txt") in
  let statement = Bj.File.read (path folder "statement.html") in
  let oracle = path folder "oracle.py" in
  let oracle = if Sys.file_exists oracle then Bj.File.read oracle else "" in
  let base = manage_path id ^ "/p/" ^ name in
  let tests = path folder "tests" in
  let count = List.length (test_numbers tests) in
  page (Bj.Kv.get h "title") ("<p>" ^ link (manage_path id) "Back to draft" ^ "</p>" ^
    problem_form base (Bj.Kv.get h "title") (Bj.Kv.get h "time") (Bj.Kv.get h "memory") statement oracle "Save" ^
    "<h2>Preview</h2>" ^ statement ^ "<h2>Hidden tests</h2><p>" ^
    string_of_int count ^ (if count = 1 then " test pair" else " test pairs") ^ " uploaded.</p>" ^
    "<p>ZIP this exact directory layout, with no surrounding folder:</p>" ^
    "<pre>tests/\n├── 1.in\n├── 1.out\n├── 2.in\n└── 2.out</pre>" ^
    "<p>Tests must be consecutive .in/.out pairs. Uploading a ZIP replaces the entire draft test set. " ^
    "Maximum ZIP size: 4 MiB; each file: 2 MiB.</p>" ^
    test_form (base ^ "/tests") ^ post_button (base ^ "/delete") "Delete problem")

let add_problem id =
  let dir = draft id in
  let title, time, memory, statement, oracle = problem_fields (form ()) in
  let rec choose letter =
    if letter > Char.code 'z' then bad "Maximum 26 problems";
    let name = String.make 1 (Char.chr letter) in
    if Sys.file_exists (path dir name) then choose (letter + 1) else name in
  let name = choose (Char.code 'a') in
  let staging = path dir (".new-" ^ Bj.Sub.id ()) in
  Unix.mkdir staging 0o700;
  Unix.mkdir (path staging "tests") 0o700;
  save_problem staging title time memory statement oracle;
  Sys.rename staging (path dir name);
  redirect (manage_path id ^ "/p/" ^ name)

let edit_problem id name =
  let dir = draft id in
  let folder = load_problem dir name in
  let title, time, memory, statement, oracle = problem_fields (form ()) in
  save_problem folder title time memory statement oracle;
  redirect (manage_path id ^ "/p/" ^ name)

let replace_tests id name =
  let dir = draft id in
  let folder = load_problem dir name in
  let fields = form ~limit:4194304 () in
  (try Test_archive.replace folder (get fields "archive") with
  | Failure message -> bad message
  | Zip.Error (_, _, message) -> bad ("Invalid ZIP: " ^ message)
  | End_of_file -> bad "Invalid ZIP");
  redirect (manage_path id ^ "/p/" ^ name)

let delete_problem id name =
  let dir = draft id in
  Bj.Admin_data.remove (load_problem dir name);
  redirect (manage_path id)

let publish id =
  let dir = draft id in
  (try ignore (Bj.Admin_data.validate (data ()) id) with
  | Failure message -> bad message
  | ex -> bad (Printexc.to_string ex));
  Array.iter (fun name -> if name.[0] = '.' then Bj.Admin_data.remove (path dir name)) (Sys.readdir dir);
  Sys.rename dir (Bj.Admin_data.published (data ()) id);
  redirect (contest_path id)

let delete_draft id =
  Bj.Admin_data.remove (draft id);
  redirect "/"

let hack_readiness id c =
  if c.Bj.Contest.problems = [] then Some "No problems to hack" else
  let missing = List.find_opt (fun p ->
    let oracle = path (path c.Bj.Contest.dir p.Bj.Contest.name) "oracle.py" in
    not (Sys.file_exists oracle && Bj.Admin_data.regular oracle)) c.problems in
  match missing with
  | Some p -> Some ("Problem " ^ p.name ^ " needs an oracle before hacks can start")
  | None ->
      let pending = path (data ()) "pending" in
      let waiting = Array.exists (fun name ->
        Bj.Sub.id_ok name && (Bj.Sub.read (path pending name)).contest = id) (Sys.readdir pending) in
      if waiting then Some "Wait for all accepted submissions to be judged" else None

let finish_readiness id =
  let hacks = path (data ()) "hacks" in
  let pending = path hacks "pending" and done_dir = path hacks "done" in
  let waiting = Array.exists (fun name ->
    Bj.Sub.id_ok name && (Bj.Hack.read (path pending name)).contest = id) (Sys.readdir pending) in
  let applying = Array.exists (fun name ->
    if not (Bj.Sub.id_ok name) then false else
    let h = Bj.Hack.read (path done_dir name) in
    h.contest = id && h.result = Some Bj.Hack.Success &&
    not (Sys.file_exists (path done_dir (name ^ ".applied")))) (Sys.readdir done_dir) in
  if waiting || applying then fail "409 Conflict" "Wait for hacks to finish"

let transition id expected next =
  let dir = Bj.Admin_data.published (data ()) (valid_id id) in
  if not (Sys.file_exists dir) then fail "404 Not Found" "Unknown contest";
  Bj.Contest.with_lock dir (fun () ->
    let c = Bj.Contest.load dir in
    if c.phase <> expected then fail "409 Conflict" "Contest phase has changed";
    if next = Bj.Contest.Hacking then (match hack_readiness id c with
      | None -> () | Some message -> fail "409 Conflict" message);
    if next = Bj.Contest.Finished then finish_readiness id;
    Bj.Contest.set_phase dir next);
  redirect (manage_path id)

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
    "<tr><td>" ^ link (manage_path id ^ "/s/" ^ s.id) s.id ^ "</td><td>" ^ escape s.author ^
    "</td><td>" ^ escape s.problem ^ "</td><td>" ^ escape status ^
    (if s.practice then " (practice)" else "") ^ "</td></tr>") recent in
  let problems = List.map (fun p -> "<li>" ^ escape (p.Bj.Contest.name ^ ": " ^ p.title) ^ "</li>") c.problems in
  let phase, action = match c.phase with
    | Bj.Contest.Ready -> "Ready", post_button (manage_path id ^ "/start") "Start contest"
    | Bj.Contest.Running -> "Accepting submissions", post_button (manage_path id ^ "/stop") "Stop submissions"
    | Bj.Contest.Stopped -> "Submissions stopped", (match hack_readiness id c with
        | None -> post_button (manage_path id ^ "/hacks/start") "Start hacks"
        | Some message -> "<p>" ^ escape message ^ "</p>")
    | Bj.Contest.Hacking -> "Accepting hacks", post_button (manage_path id ^ "/hacks/stop") "Stop hacks"
    | Bj.Contest.Finished -> "Finished", "" in
  let hacks kind =
    let folder = path (path (data ()) "hacks") kind in
    List.filter_map (fun name ->
      if not (Bj.Sub.id_ok name) then None else try
        let h = Bj.Hack.read (path folder name) in
        if h.contest = id then Some h else None
      with _ -> None) (Array.to_list (Sys.readdir folder)) in
  let hack_rows = List.map (fun (h : Bj.Hack.t) ->
    let applied = path (path (path (data ()) "hacks") "done") (h.id ^ ".applied") in
    let result = if h.result = Some Bj.Hack.Success && not (Sys.file_exists applied) then "pending"
      else match h.result with None -> "pending" | Some r -> Bj.Hack.string_of_outcome r in
    "<li>" ^ escape (h.author ^ " → " ^ h.target ^ ": " ^ result) ^ "</li>")
    (List.append (hacks "pending") (hacks "done")) in
  page c.title ("<p>Contest link: " ^ link (contest_path id) (contest_path id) ^ "</p>" ^
    "<p>" ^ phase ^ "</p>" ^ action ^
    "<p>" ^ string_of_int (List.length (List.filter (fun s -> not s.Bj.Sub.practice) all)) ^
    " official submissions; " ^
    string_of_int (List.length (List.filter (fun s -> s.Bj.Sub.practice) all)) ^ " practice; " ^
    string_of_int (List.length (List.filter (fun s -> s.Bj.Sub.result = None) all)) ^ " pending.</p>" ^
    "<h2>Problems</h2><ol>" ^ String.concat "" problems ^ "</ol><h2>Recent submissions</h2>" ^
    "<table><tr><th>ID</th><th>Name</th><th>Problem</th><th>Verdict</th></tr>" ^
    String.concat "" rows ^ "</table><h2>Hacks</h2><ul>" ^ String.concat "" hack_rows ^ "</ul>")

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
  page "Submission" ("<p>" ^ link (manage_path id) "Back to contest" ^ "</p>" ^
    "<p>" ^ escape (s.author ^ " / " ^ s.problem ^ " / " ^
      (if s.practice then "practice / " else "") ^ result) ^ "</p>" ^
    "<h2>Source</h2><pre>" ^ escape s.source ^ "</pre>" ^ error)

let view_contest id =
  let id = valid_id id in
  let draft = Bj.Admin_data.draft (data ()) id in
  let live = Bj.Admin_data.published (data ()) id in
  if Sys.file_exists draft then draft_page id draft
  else if Sys.file_exists live then published_page id live
  else fail "404 Not Found" "Unknown contest"

let handle parts =
  ignore (admin ());
  match env "REQUEST_METHOD", parts with
  | "GET", [ "users" ] -> list_users ()
  | "GET", [ "users"; name ] -> user_page name
  | "POST", [ "users"; name; "reset" ] -> reset_user name
  | "GET", [ "new" ] -> page "New contest" (contest_form "/contests/new" "" "Create")
  | "POST", [ "new" ] -> new_contest ()
  | "GET", [ id; "manage" ] -> view_contest id
  | "POST", [ id; "manage" ] -> update_contest id
  | "POST", [ id; "manage"; "p" ] -> add_problem id
  | "GET", [ id; "manage"; "p"; name ] -> draft_problem id (letter name)
  | "POST", [ id; "manage"; "p"; name ] -> edit_problem id name
  | "POST", [ id; "manage"; "p"; name; "tests" ] -> replace_tests id name
  | "POST", [ id; "manage"; "p"; name; "delete" ] -> ignore (form ()); delete_problem id name
  | "POST", [ id; "manage"; "publish" ] -> ignore (form ()); publish id
  | "POST", [ id; "manage"; "start" ] -> ignore (form ()); transition id Bj.Contest.Ready Bj.Contest.Running
  | "POST", [ id; "manage"; "stop" ] -> ignore (form ()); transition id Bj.Contest.Running Bj.Contest.Stopped
  | "POST", [ id; "manage"; "hacks"; "start" ] -> ignore (form ()); transition id Bj.Contest.Stopped Bj.Contest.Hacking
  | "POST", [ id; "manage"; "hacks"; "stop" ] -> ignore (form ()); transition id Bj.Contest.Hacking Bj.Contest.Finished
  | "POST", [ id; "manage"; "delete" ] -> ignore (form ()); delete_draft id
  | "GET", [ id; "manage"; "s"; sid ] -> view_submission (valid_id id) sid
  | _ -> fail "404 Not Found" "Not found"
