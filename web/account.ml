let path = Filename.concat
let env name = try Sys.getenv name with Not_found -> ""
let data () = Sys.getenv "OPENHOST_APP_DATA_DIR"
let accounts () = path (data ()) "accounts"
let sessions () = path (data ()) "sessions"
let joined () = path (data ()) "joined"
let domain () = match Sys.getenv_opt "JUDGE_DOMAIN" with
  | Some domain -> domain
  | None -> "judge.1.bottle.cloud"
let author username = username ^ "@" ^ domain ()
let lifetime = 30 * 24 * 60 * 60

type t = { username : string; author : string; csrf : string; admin : bool }
let remote : t option option ref = ref None

let username_ok name =
  let n = String.length name in
  n >= 3 && n <= 24 && String.for_all (function
    | 'a'..'z' | '0'..'9' | '_' | '-' -> true | _ -> false) name

let normalize name = String.lowercase_ascii name

let password_ok password =
  let n = String.length password in n >= 12 && n <= 128 && not (String.contains password '\000')

let random n =
  let ic = open_in_bin "/dev/urandom" in
  let s = really_input_string ic n in
  close_in ic;
  View.hex s

let hash mode input =
  let in_read, in_write = Unix.pipe () in
  let out_read, out_write = Unix.pipe () in
  Unix.set_close_on_exec in_write;
  Unix.set_close_on_exec out_read;
  let pid = Unix.create_process "python3" [| "python3"; "/src/web/password.py"; mode |]
    in_read out_write Unix.stderr in
  Unix.close in_read;
  Unix.close out_write;
  let oc = Unix.out_channel_of_descr in_write in
  output_string oc input;
  close_out oc;
  let ic = Unix.in_channel_of_descr out_read in
  let answer = try input_line ic with End_of_file -> "" in
  close_in ic;
  let _, status = Unix.waitpid [] pid in
  if status <> Unix.WEXITED 0 then failwith "Password hashing failed";
  answer

let account_file username = path (accounts ()) username

let read username =
  let fields, _ = Bj.Kv.read (account_file username) in
  Bj.Kv.get fields "hash", Bj.Kv.get fields "epoch", Bj.Kv.get fields "admin" = "yes"

let write username password epoch admin =
  Bj.Admin_data.write (account_file username)
    ("hash: " ^ password ^ "\nepoch: " ^ epoch ^
     "\nadmin: " ^ (if admin then "yes" else "no") ^ "\n\n") 0o600

let with_lock f =
  let fd = Unix.openfile (path (accounts ()) ".lock") [ Unix.O_CREAT; Unix.O_RDWR ] 0o600 in
  Fun.protect ~finally:(fun () -> Unix.close fd) (fun () ->
    Unix.lockf fd Unix.F_LOCK 0;
    f ())

let cookie token secure =
  "judge_session=" ^ token ^ "; Path=/; Max-Age=" ^ string_of_int lifetime ^
  "; HttpOnly; SameSite=Lax" ^
  (if secure then "; Secure" else "")

let clear_cookie secure =
  "judge_session=; Path=/; Max-Age=0; HttpOnly; SameSite=Lax" ^
  (if secure then "; Secure" else "")

let valid_token token =
  String.length token = 64 && String.for_all (function '0'..'9' | 'a'..'f' -> true | _ -> false) token

let token () =
  let cookies = try Sys.getenv "HTTP_COOKIE" with Not_found -> "" in
  List.find_map (fun field ->
    let field = String.trim field in
    if String.starts_with ~prefix:"judge_session=" field then
      let value = String.sub field 14 (String.length field - 14) in
      if valid_token value then Some value else None
    else None) (String.split_on_char ';' cookies)

let local () =
  match token () with
  | None -> None
  | Some token ->
      try
        let fields, _ = Bj.Kv.read (path (sessions ()) token) in
        let get = Bj.Kv.get fields in
        let username = get "username" in
        if not (username_ok username) || int_of_string (get "expires") <= int_of_float (Unix.time ()) then None
        else let _, epoch, admin = read username in
          if not (Bj.Contest.same epoch (get "epoch")) then None
          else Some { username; author = author username; csrf = get "csrf"; admin }
      with _ -> None

let current () = match !remote with Some user -> user | None -> local ()

let new_session username epoch =
  let token = random 32 and csrf = random 16 in
  Bj.Kv.write (path (sessions ()) token)
    [ "username", username; "epoch", epoch;
      "expires", string_of_int (int_of_float (Unix.time ()) + lifetime);
      "csrf", csrf ] "";
  Unix.chmod (path (sessions ()) token) 0o600;
  token

let signup name password =
  let username = normalize name in
  if not (username_ok username) then invalid_arg "Username must be 3 to 24 letters, digits, _ or -";
  if not (password_ok password) then invalid_arg "Password must be 12 to 128 bytes";
  let encrypted = hash "hash" password in
  let epoch = random 16 in
  with_lock (fun () ->
    if Sys.file_exists (account_file username) then invalid_arg "Username already taken";
    let admin = not (Array.exists username_ok (Sys.readdir (accounts ()))) in
    write username encrypted epoch admin);
  new_session username epoch

let login name password =
  let username = normalize name in
  if not (username_ok username) || not (password_ok password) ||
     not (Sys.file_exists (account_file username)) then None
  else
    let encrypted, epoch, _ = read username in
    if hash "verify" (encrypted ^ "\n" ^ password) = "yes" then
      Some (new_session username epoch)
    else None

let logout () = match token () with
  | None -> ()
  | Some token ->
      (try Unix.unlink (path (sessions ()) token)
       with Unix.Unix_error (Unix.ENOENT, _, _) -> ())

let reset name password =
  let username = normalize name in
  if not (username_ok username) then invalid_arg "Invalid username";
  if not (password_ok password) then invalid_arg "Password must be 12 to 128 bytes";
  let encrypted = hash "hash" password in
  with_lock (fun () ->
    if not (Sys.file_exists (account_file username)) then invalid_arg "Unknown user";
    let _, _, admin = read username in
    write username encrypted (random 16) admin)

let users () =
  List.sort String.compare (List.filter username_ok (Array.to_list (Sys.readdir (accounts ()))))

let navigation user =
  "<nav>" ^ View.link "/" "Contests" ^ " | " ^ View.link "/problemset" "Problemset" ^
  " | " ^ View.link "/join" "Join contest" ^
  (match user with
  | None -> " | " ^ View.link "/login" "Sign in" ^ " | " ^ View.link "/signup" "Sign up"
  | Some account -> " | " ^ View.link "/account" account.author ^
      (if account.admin then " | " ^ View.link "/contests/new" "Create contest" ^
        " | " ^ View.link "/users" "Manage users" else "")) ^ "</nav>"

let joins username =
  let file = path (joined ()) username in
  if not (Sys.file_exists file) then [] else
  List.filter_map (fun line -> match String.split_on_char '/' line with
    | [ host; id ] when Bj.Contest.id_ok id -> Some (host, id)
    | _ -> None) (String.split_on_char '\n' (Bj.File.read file))

let add_join user host id =
  with_lock (fun () ->
    let old = joins user.username in
    if not (List.mem (host, id) old) then
      Bj.Admin_data.write (path (joined ()) user.username)
        (String.concat "" (List.map (fun (host, id) -> host ^ "/" ^ id ^ "\n")
           (old @ [ host, id ]))) 0o600)

let send ?(headers = []) status title body =
  View.send ~headers status "text/html; charset=utf-8" (View.page title (navigation (local ()) ^ body))

let redirect ?(headers = []) url =
  View.send ~headers:(("Location", url) :: headers) "303 See Other" "text/html; charset=utf-8" ""

let next_path value =
  let ok = match String.split_on_char '/' value with
    | [ "contests"; id ] -> Bj.Contest.id_ok id
    | [ "contests"; host; id ] ->
        String.length host <= 120 && String.contains host '.' &&
        String.for_all (function 'a'..'z' | '0'..'9' | '-' | '.' -> true | _ -> false) host &&
        Bj.Contest.id_ok id
    | _ -> false in
  if ok then value else ""

let next_from_query () =
  let query = env "QUERY_STRING" in
  match String.split_on_char '=' query with
  | [ "next"; value ] -> next_path value
  | _ -> ""

let destination next = if next = "" then "/" else "/" ^ next

let form action next label =
  "<form method=\"post\" enctype=\"multipart/form-data\" action=\"/" ^ action ^ "\">" ^
  "<p><label>Username <input name=\"username\" required minlength=\"3\" maxlength=\"24\" pattern=\"[A-Za-z0-9_-]{3,24}\" autocomplete=\"username\"></label></p>" ^
  "<p><label>Password <input name=\"password\" type=\"password\" required minlength=\"12\" maxlength=\"128\" autocomplete=\"" ^
    (if action = "signup" then "new-password" else "current-password") ^ "\"></label></p>" ^
  "<input type=\"hidden\" name=\"next\" value=\"" ^ next ^ "\">" ^
  "<button>" ^ label ^ "</button></form>"

let current_page account =
  let entries = List.map (fun (host, id) ->
    "<li>" ^ View.link ("/contests/" ^ host ^ "/" ^ id) (host ^ " #" ^ id) ^ "</li>")
    (joins account.username) in
  send "200 OK" "Account" ("<p>Signed in as " ^ View.escape account.author ^ ".</p>" ^
    "<h2>Joined contests</h2><ul>" ^ String.concat "" entries ^ "</ul>" ^
    "<form method=\"post\" enctype=\"multipart/form-data\" action=\"/logout\">" ^
    "<input type=\"hidden\" name=\"csrf\" value=\"" ^ account.csrf ^ "\">" ^
    "<button>Sign out</button></form>")

let handle name =
  let next = next_from_query () in
  match env "REQUEST_METHOD", name with
  | "GET", "signup" ->
      send "200 OK" "Sign up" (form "signup" next "Create account" ^
        "<p>Already registered? " ^ View.link ("/login" ^ (if next = "" then "" else "?next=" ^ next)) "Sign in" ^ "</p>")
  | "GET", "login" ->
      send "200 OK" "Sign in" (form "login" next "Sign in" ^
        "<p>New here? " ^ View.link ("/signup" ^ (if next = "" then "" else "?next=" ^ next)) "Create account" ^ "</p>")
  | "GET", "account" ->
      (match local () with Some user -> current_page user | None -> redirect "/login")
  | "POST", "signup" ->
      let fields = Bj.Form.read (env "CONTENT_TYPE") (env "CONTENT_LENGTH") in
      let username = Bj.Form.get fields "username" and password = Bj.Form.get fields "password" in
      let next = next_path (Bj.Form.get fields "next") in
      let token = signup username password in
      redirect ~headers:[ "Set-Cookie", cookie token (env "HTTP_X_FORWARDED_PROTO" = "https") ]
        (destination next)
  | "POST", "login" ->
      let fields = Bj.Form.read (env "CONTENT_TYPE") (env "CONTENT_LENGTH") in
      let username = Bj.Form.get fields "username" and password = Bj.Form.get fields "password" in
      let next = next_path (Bj.Form.get fields "next") in
      (match login username password with
      | Some token ->
          redirect ~headers:[ "Set-Cookie", cookie token (env "HTTP_X_FORWARDED_PROTO" = "https") ]
            (destination next)
      | None -> send "401 Unauthorized" "Sign in" ("<p>Incorrect username or password.</p>" ^ form "login" next "Sign in"))
  | "POST", "logout" ->
      let user = match local () with
        | Some user -> user | None -> invalid_arg "Sign in first" in
      let fields = Bj.Form.read (env "CONTENT_TYPE") (env "CONTENT_LENGTH") in
      if not (Bj.Contest.same (Bj.Form.get fields "csrf") user.csrf) then invalid_arg "Invalid form token";
      logout ();
      redirect ~headers:[ "Set-Cookie", clear_cookie (env "HTTP_X_FORWARDED_PROTO" = "https") ] "/login"
  | _ -> send "404 Not Found" "Not found" "<p>Not found.</p>"
