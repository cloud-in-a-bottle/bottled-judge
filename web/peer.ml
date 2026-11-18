let path = Filename.concat
let env name = try Sys.getenv name with Not_found -> ""
let data () = Sys.getenv "OPENHOST_APP_DATA_DIR"
let peers () = path (data ()) "peers"
let routes () = path (data ()) "remote"
let request : string option ref = ref None

let with_request_lock f =
  let fd = Unix.openfile (path (peers ()) ".requests.lock") [ Unix.O_CREAT; Unix.O_RDWR ] 0o600 in
  Fun.protect ~finally:(fun () -> Unix.close fd) (fun () ->
    Unix.lockf fd Unix.F_LOCK 0;
    f ())

let name_ok name =
  String.length name <= 120 && match String.split_on_char '.' name with
  | _ :: _ :: _ as labels ->
      List.for_all (fun label ->
        let n = String.length label in
        n > 0 && n <= 63 && label.[0] <> '-' && label.[n - 1] <> '-' &&
        String.for_all (function 'a'..'z' | '0'..'9' | '-' -> true | _ -> false) label) labels
  | _ -> false

let key_ok key = String.length key = 64 && String.for_all (function
  | '0'..'9' | 'a'..'f' -> true | _ -> false) key

let url_ok name url = url = "https://" ^ name ||
  (env "JUDGE_ALLOW_HTTP_PEERS" = "1" && String.starts_with ~prefix:"http://" url &&
   not (String.contains url '\n') && String.length url < 200)

let peer_file name = path (peers ()) name

let add name url key =
  if not (name_ok name) || name = Account.domain () then invalid_arg "Invalid peer domain";
  if not (url_ok name url) then invalid_arg "Invalid peer URL";
  if not (key_ok key) then invalid_arg "Invalid peer key";
  Bj.Admin_data.write (peer_file name) ("url: " ^ url ^ "\nkey: " ^ key ^ "\n\n") 0o600

let list () = List.sort compare (List.filter name_ok (Array.to_list (Sys.readdir (peers ()))))

let connect name token =
  if Bj.Contest.find (path (data ()) "contests") token <> None then invalid_arg "Local contest already exists";
  let file = path (routes ()) token in
  if Sys.file_exists file then begin
    if String.trim (Bj.File.read file) <> name then invalid_arg "Contest already followed"
  end else Bj.Admin_data.write file (name ^ "\n") 0o600

let join url =
  let matches = List.filter_map (fun name ->
    let fields, _ = Bj.Kv.read (peer_file name) in
    let prefix = Bj.Kv.get fields "url" ^ "/c/" in
    if not (String.starts_with ~prefix url) then None else
      let rest = String.sub url (String.length prefix) (String.length url - String.length prefix) in
      let token = if String.ends_with ~suffix:"/" rest then String.sub rest 0 (String.length rest - 1) else rest in
      if Account.next_token token = token && token <> "" then Some (name, token) else None) (list ()) in
  match matches with
  | [ name, token ] -> name, token
  | _ -> invalid_arg "Invalid contest URL or unpaired host"

let remote token =
  let file = path (routes ()) token in
  if Sys.file_exists file then Some (String.trim (Bj.File.read file)) else None

let read_body () =
  let n = try int_of_string (env "CONTENT_LENGTH") with _ -> invalid_arg "Invalid request length" in
  if n < 0 || n > 131072 then invalid_arg "Request too large";
  try really_input_string stdin n with End_of_file -> invalid_arg "Truncated request"

let run mode body values direct =
  let input, output = Unix.pipe () in
  Unix.set_close_on_exec output;
  let result, sink = if direct then Unix.stdout, None else
    let r, w = Unix.pipe () in
    Unix.set_close_on_exec r;
    w, Some r in
  let extra = Array.of_list (List.map (fun (k, v) -> k ^ "=" ^ v) values) in
  let pid = Unix.create_process_env "python3" [| "python3"; "/src/web/peer.py"; mode |]
    (Array.append (Unix.environment ()) extra) input result Unix.stderr in
  Unix.close input;
  (match sink with Some _ -> Unix.close result | None -> ());
  let oc = Unix.out_channel_of_descr output in
  output_string oc body;
  close_out oc;
  let response = match sink with
    | None -> ""
    | Some fd ->
        let ic = Unix.in_channel_of_descr fd in
        let value = try input_line ic with End_of_file -> "" in
        close_in ic;
        value in
  let _, status = Unix.waitpid [] pid in
  response, status

let handle_join () =
  let page body = View.send "200 OK" "text/html; charset=utf-8" (View.page "Join contest" body) in
  match env "REQUEST_METHOD", Account.local () with
  | "GET", None -> page ("<p>" ^ View.link "/c/login" "Sign in" ^ " or " ^
      View.link "/c/signup" "create an account" ^ " to join a contest.</p>")
  | "GET", Some user ->
      page ("<form method=\"post\" enctype=\"multipart/form-data\" action=\"/c/join\">" ^
        "<input type=\"hidden\" name=\"csrf\" value=\"" ^ user.csrf ^ "\">" ^
        "<p><label>Contest URL <input type=\"url\" name=\"url\" required placeholder=\"https://judge.example/c/...\"></label></p>" ^
        "<button>Join contest</button></form>")
  | "POST", Some user ->
      let fields = Bj.Form.read (env "CONTENT_TYPE") (env "CONTENT_LENGTH") in
      if not (Bj.Contest.same (Bj.Form.get fields "csrf") user.csrf) then invalid_arg "Invalid form token";
      let name, token = join (String.trim (Bj.Form.get fields "url")) in
      let values = [ "FED_PEER", name; "FED_METHOD", "GET";
        "FED_PATH", "/c/f/v1/" ^ token; "FED_AUTHOR", user.author;
        "FED_CSRF", user.csrf; "FED_REQUEST", ""; "FED_CONTENT_TYPE", "" ] in
      let response, status = run "check" "" values false in
      if status <> Unix.WEXITED 0 || response <> "ok" then invalid_arg "Contest link unavailable";
      connect name token;
      View.send ~headers:[ "Location", "/c/" ^ token ] "303 See Other" "text/html; charset=utf-8" ""
  | "POST", None -> View.send "401 Unauthorized" "text/html; charset=utf-8"
      (View.page "Sign in" ("<p>" ^ View.link "/c/login" "Sign in" ^ " first.</p>"))
  | _ -> View.send "405 Method Not Allowed" "text/html; charset=utf-8" (View.page "Not allowed" "")

let incoming () =
  let body = if env "REQUEST_METHOD" = "POST" then read_body () else "" in
  let response, status = run "verify" body [] false in
  if response <> "ok" || status <> Unix.WEXITED 0 then invalid_arg "Invalid federation signature";
  let author = env "HTTP_X_JUDGE_AUTHOR" in
  let csrf = env "HTTP_X_JUDGE_CSRF" in
  let account = if author = "" then None else Some { Account.author; csrf } in
  account, env "HTTP_X_JUDGE_REQUEST", body

let forward token tail =
  let peer = match remote token with Some peer -> peer | None -> invalid_arg "Unknown remote contest" in
  let method_ = env "REQUEST_METHOD" in
  if method_ <> "GET" && method_ <> "POST" then invalid_arg "Unsupported method";
  let account = Account.local () in
  let body, request = if method_ = "POST" then begin
    if tail <> [ "submit" ] && tail <> [ "hack" ] then invalid_arg "Unsupported action";
    let user = match account with Some user -> user | None -> invalid_arg "Sign in first" in
    let body = read_body () in
    let fields = Bj.Form.parse (env "CONTENT_TYPE") body in
    if not (Bj.Contest.same (Bj.Form.get fields "csrf") user.csrf) then invalid_arg "Invalid form token";
    let id = Bj.Form.get fields "request" in
    if not (Bj.Sub.id_ok id) then invalid_arg "Invalid request ID";
    body, id
  end else "", "" in
  let endpoint = "/c/f/v1/" ^ token ^
    (if tail = [] then "" else "/" ^ String.concat "/" tail) in
  let values = [ "FED_PEER", peer; "FED_METHOD", method_;
    "FED_PATH", endpoint; "FED_AUTHOR", (match account with Some user -> user.author | None -> "");
    "FED_CSRF", (match account with Some user -> user.csrf | None -> "");
    "FED_REQUEST", request; "FED_CONTENT_TYPE", env "CONTENT_TYPE" ] in
  let _, status = run "send" body values true in
  if status <> Unix.WEXITED 0 then prerr_endline "Federation request failed"
