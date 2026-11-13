let escape s =
  let b = Buffer.create (String.length s) in
  String.iter (function
    | '&' -> Buffer.add_string b "&amp;"
    | '<' -> Buffer.add_string b "&lt;"
    | '>' -> Buffer.add_string b "&gt;"
    | '"' -> Buffer.add_string b "&quot;"
    | '\'' -> Buffer.add_string b "&#39;"
    | c -> Buffer.add_char b c) s;
  Buffer.contents b

let link url label = "<a href=\"" ^ escape url ^ "\">" ^ escape label ^ "</a>"

let page title body =
  "<!doctype html><html lang=\"en\"><head><meta charset=\"utf-8\"><meta name=\"viewport\" content=\"width=device-width, initial-scale=1\"><title>" ^
  escape title ^ "</title></head><body><h1>" ^ escape title ^ "</h1>" ^ body ^ "</body></html>"

let send ?(headers = []) status kind body =
  Printf.printf "Status: %s\r\nContent-Type: %s\r\nCache-Control: no-store\r\nReferrer-Policy: no-referrer\r\nX-Content-Type-Options: nosniff\r\nContent-Security-Policy: default-src 'none'; style-src 'none'; script-src 'none'; form-action 'self'; base-uri 'none'; frame-ancestors 'none'\r\n" status kind;
  List.iter (fun (k, v) -> Printf.printf "%s: %s\r\n" k v) headers;
  Printf.printf "\r\n%s" body

let hex s =
  let b = Buffer.create (2 * String.length s) in
  String.iter (fun c -> Printf.bprintf b "%02x" (Char.code c)) s;
  Buffer.contents b

let unhex s =
  let n = String.length s in
  if n = 0 || n > 64 || n mod 2 <> 0 then None
  else try
    let b = Bytes.create (n / 2) in
    let rec decode i =
      if i < n / 2 then begin
        Bytes.set b i (Char.chr (int_of_string ("0x" ^ String.sub s (2 * i) 2)));
        decode (i + 1)
      end
    in
    decode 0;
    Some (Bytes.to_string b)
  with _ -> None

let name_cookie () =
  let value = try Sys.getenv "HTTP_COOKIE" with Not_found -> "" in
  let pairs = List.map String.trim (String.split_on_char ';' value) in
  let pair = List.find_opt (String.starts_with ~prefix:"judge_name=") pairs in
  match pair with None -> None | Some p -> unhex (String.sub p 11 (String.length p - 11))

let cookie token name secure =
  "judge_name=" ^ hex name ^ "; Path=/c/" ^ token ^ "/; HttpOnly; SameSite=Lax" ^
  (if secure then "; Secure" else "")
