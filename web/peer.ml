let path = Filename.concat
let env name = try Sys.getenv name with Not_found -> ""
let data () = Sys.getenv "OPENHOST_APP_DATA_DIR"
let keys () = path (data ()) "keys"
let request : string option ref = ref None

let with_request_lock f =
  let fd = Unix.openfile (path (keys ()) ".requests.lock") [ Unix.O_CREAT; Unix.O_RDWR ] 0o600 in
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

let join url =
  let parts = String.split_on_char '/' url in
  let scheme, host, id = match parts with
    | [ scheme; ""; host; "contests"; id ]
    | [ scheme; ""; host; "contests"; id; "" ] -> scheme, host, id
    | _ -> invalid_arg "Invalid contest URL" in
  let host = if scheme = "http:" && env "JUDGE_ALLOW_HTTP_PEERS" = "1" &&
    String.ends_with ~suffix:":8080" host then
      String.sub host 0 (String.length host - 5) else
    if scheme = "https:" then host else invalid_arg "Invalid contest URL" in
  if not (name_ok host && Bj.Contest.id_ok id) then invalid_arg "Invalid contest URL";
  host, id

let read_body () =
  let n = try int_of_string (env "CONTENT_LENGTH") with _ -> invalid_arg "Invalid request length" in
  if n < 0 || n > 131072 then invalid_arg "Request too large";
  try really_input_string stdin n with End_of_file -> invalid_arg "Truncated request"

let key_file () = path (keys ()) "private"

let private_key () =
  let key = Bj.File.read (key_file ()) in
  if String.length key <> 32 then failwith "Invalid instance key";
  key

let valid_hex n text =
  String.length text = n && String.for_all (function '0'..'9' | 'a'..'f' -> true | _ -> false) text

let header value = not (String.contains value '\r' || String.contains value '\n')

let fetch peer method_ endpoint body headers =
  if not (name_ok peer) then invalid_arg "Invalid instance domain";
  let c = Curl.init () in
  Fun.protect ~finally:(fun () -> Curl.cleanup c) (fun () ->
    let http = env "JUDGE_ALLOW_HTTP_PEERS" = "1" in
    Curl.set_url c ((if http then "http://" else "https://") ^ peer ^
      (if http then ":8080" else "") ^ endpoint);
    Curl.set_connecttimeout c 5;
    Curl.set_timeout c 15;
    Curl.set_proxy c "";
    Curl.set_sslverifypeer c true;
    Curl.set_sslverifyhost c Curl.SSLVERIFYHOST_HOSTNAME;
    Curl.set_followlocation c false;
    Curl.set_httpheader c headers;
    if method_ = "POST" then (Curl.set_post c true; Curl.set_postfields c body;
      Curl.set_postfieldsize c (String.length body));
    let content = Buffer.create 4096 in
    let too_large = ref false in
    let response_headers = ref [] in
    Curl.set_writefunction c (fun chunk ->
      if Buffer.length content + String.length chunk > 4194304 then (too_large := true; 0)
      else (Buffer.add_string content chunk; String.length chunk));
    Curl.set_headerfunction c (fun line ->
      (if String.starts_with ~prefix:"HTTP/" line then response_headers := [] else
        match String.index_opt line ':' with
        | None -> ()
        | Some i ->
            let name = String.lowercase_ascii (String.sub line 0 i) in
            if List.mem name [ "content-type"; "location"; "refresh" ] then begin
              let value = String.trim (String.sub line (i+1) (String.length line-i-1)) in
              let name = match name with "content-type" -> "Content-Type"
                | "location" -> "Location" | _ -> "Refresh" in
              if header value then response_headers := (name, value) :: !response_headers
            end);
      String.length line);
    (try Curl.perform c with ex -> if !too_large then failwith "Response too large" else raise ex);
    Curl.get_responsecode c, List.rev !response_headers, Buffer.contents content)

let message method_ endpoint author csrf timestamp request content_type body =
  String.concat "\n" [ method_; endpoint; author; csrf; timestamp; request; content_type;
    Bj.Crypto.hex (Bj.Crypto.sha256 body) ]

let public_of peer =
  let code, _, body = fetch peer "GET" "/api/federation/v1/key" "" [] in
  if code <> 200 || String.length body <> 65 || body.[64] <> '\n' then
    invalid_arg "Invalid peer key response";
  let hex = String.sub body 0 64 in
  if not (valid_hex 64 hex) then invalid_arg "Invalid peer key";
  Bj.Crypto.unhex hex

let verify body =
  let name = env "HTTP_X_JUDGE_PEER" and author = env "HTTP_X_JUDGE_AUTHOR" in
  let csrf = env "HTTP_X_JUDGE_CSRF" and timestamp = env "HTTP_X_JUDGE_TIME" in
  let request = env "HTTP_X_JUDGE_REQUEST" and signature = env "HTTP_X_JUDGE_SIGNATURE" in
  let method_ = env "REQUEST_METHOD" in
  let endpoint = List.hd (String.split_on_char '?' (env "REQUEST_URI")) in
  let time = try int_of_string timestamp with _ -> invalid_arg "Invalid timestamp" in
  if env "QUERY_STRING" <> "" || String.length body > 131072 ||
     abs (int_of_float (Unix.time ()) - time) > 300 ||
     not (valid_hex 128 signature && name_ok name) ||
     (author <> "" && (match String.split_on_char '@' author with
       | [ username; domain ] -> domain <> name || String.length username < 3 ||
           String.length username > 24 || not (String.for_all (function
             'a'..'z' | '0'..'9' | '_' | '-' -> true | _ -> false) username)
       | _ -> true)) ||
     (author = "" && csrf <> "") ||
     (author <> "" && not (valid_hex 32 csrf)) ||
     (method_ = "POST" && not (Bj.Sub.id_ok request)) ||
     (method_ = "GET" && request <> "") ||
     (method_ <> "GET" && method_ <> "POST") then invalid_arg "Invalid request";
  let signed = message method_ endpoint author csrf timestamp request (env "CONTENT_TYPE") body in
  if not (Bj.Crypto.verify (public_of name) signed (Bj.Crypto.unhex signature)) then
    invalid_arg "Invalid federation signature"

let send peer method_ endpoint body author csrf request content_type =
  let timestamp = string_of_int (int_of_float (Unix.time ())) in
  let signed = message method_ endpoint author csrf timestamp request
    (if method_ = "POST" then content_type else "") body in
  let signature = Bj.Crypto.hex (Bj.Crypto.sign (private_key ()) signed) in
  let headers = [ "X-Judge-Peer: " ^ Account.domain ();
    "X-Judge-Author: " ^ author; "X-Judge-Csrf: " ^ csrf;
    "X-Judge-Time: " ^ timestamp; "X-Judge-Request: " ^ request;
    "X-Judge-Signature: " ^ signature ] @
    (if method_ = "POST" then [ "Content-Type: " ^ content_type ] else []) in
  fetch peer method_ endpoint body headers

let public_key () =
  View.send "200 OK" "text/plain; charset=utf-8"
    (Bj.Crypto.hex (Bj.Crypto.public_key (private_key ())) ^ "\n")

let handle_join () =
  let page body = View.send "200 OK" "text/html; charset=utf-8"
    (View.page "Join contest" (Account.navigation (Account.local ()) ^ body)) in
  match env "REQUEST_METHOD", Account.local () with
  | "GET", None -> page ("<p>" ^ View.link "/login" "Sign in" ^ " or " ^
      View.link "/signup" "create an account" ^ " to join a contest.</p>")
  | "GET", Some user ->
      page ("<form method=\"post\" enctype=\"multipart/form-data\" action=\"/join\">" ^
        "<input type=\"hidden\" name=\"csrf\" value=\"" ^ user.csrf ^ "\">" ^
        "<p><label>Contest URL <input type=\"url\" name=\"url\" required placeholder=\"https://judge.example/contests/42\"></label></p>" ^
        "<button>Join contest</button></form>")
  | "POST", Some user ->
      let fields = Bj.Form.read (env "CONTENT_TYPE") (env "CONTENT_LENGTH") in
      if not (Bj.Contest.same (Bj.Form.get fields "csrf") user.csrf) then invalid_arg "Invalid form token";
      let name, id = join (String.trim (Bj.Form.get fields "url")) in
      let destination = if name = Account.domain () then begin
        if Bj.Contest.find (path (data ()) "contests") id = None then invalid_arg "Contest not found";
        "/contests/" ^ id
      end else begin
        let available = try
          let code, _, _ = send name "GET" ("/api/federation/v1/contests/" ^ id)
            "" user.author user.csrf "" "" in
          code = 200 with _ -> false in
        if not available then invalid_arg "Contest link unavailable";
        Account.add_join user name id;
        "/contests/" ^ name ^ "/" ^ id
      end in
      View.send ~headers:[ "Location", destination ] "303 See Other" "text/html; charset=utf-8" ""
  | "POST", None -> View.send "401 Unauthorized" "text/html; charset=utf-8"
      (View.page "Sign in" ("<p>" ^ View.link "/login" "Sign in" ^ " first.</p>"))
  | _ -> View.send "405 Method Not Allowed" "text/html; charset=utf-8" (View.page "Not allowed" "")

let incoming () =
  let body = if env "REQUEST_METHOD" = "POST" then read_body () else "" in
  verify body;
  let author = env "HTTP_X_JUDGE_AUTHOR" in
  let csrf = env "HTTP_X_JUDGE_CSRF" in
  let account = if author = "" then None else Some { Account.username = ""; author; csrf; admin = false } in
  account, env "HTTP_X_JUDGE_REQUEST", body

let forward peer id tail =
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
  let endpoint = "/api/federation/v1/contests/" ^ id ^
    (if tail = [] then "" else "/" ^ String.concat "/" tail) in
  let author, csrf = match account with
    | Some user -> user.author, user.csrf | None -> "", "" in
  try
    let status, headers, content = send peer method_ endpoint body author csrf request (env "CONTENT_TYPE") in
    let reason = match status with
      | 200 -> "OK" | 303 -> "See Other" | 400 -> "Bad Request" | 401 -> "Unauthorized"
      | 403 -> "Forbidden" | 404 -> "Not Found" | 409 -> "Conflict" | 500 -> "Internal Server Error"
      | _ -> "Response" in
    let kind = match List.assoc_opt "Content-Type" headers with
      | Some kind -> kind | None -> "text/plain; charset=utf-8" in
    let headers = List.filter (fun (name, _) -> name <> "Content-Type") headers in
    View.send ~headers (string_of_int status ^ " " ^ reason) kind content
  with _ -> View.send "502 Bad Gateway" "text/plain; charset=utf-8" "Contest host unavailable"
