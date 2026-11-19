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
  "<!doctype html><html lang=\"en\"><head><meta charset=\"utf-8\"><meta name=\"viewport\" content=\"width=device-width, initial-scale=1\"><link rel=\"stylesheet\" href=\"/theme.css\"><title>" ^
  escape title ^ "</title></head><body><h1>" ^ escape title ^ "</h1>" ^ body ^ "</body></html>"

let send ?(headers = []) status kind body =
  Printf.printf "Status: %s\r\nContent-Type: %s\r\nCache-Control: no-store\r\nReferrer-Policy: no-referrer\r\nX-Content-Type-Options: nosniff\r\nContent-Security-Policy: default-src 'none'; style-src 'self'; script-src 'none'; form-action 'self'; base-uri 'none'; frame-ancestors 'none'\r\n" status kind;
  List.iter (fun (k, v) -> Printf.printf "%s: %s\r\n" k v) headers;
  Printf.printf "\r\n%s" body

let hex s =
  let b = Buffer.create (2 * String.length s) in
  String.iter (fun c -> Printf.bprintf b "%02x" (Char.code c)) s;
  Buffer.contents b
