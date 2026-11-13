let bad () = invalid_arg "invalid multipart form"

let search s part from =
  let n = String.length part in
  let rec go i =
    if i + n > String.length s then raise Not_found
    else if s.[i] = part.[0] && String.sub s i n = part then i
    else go (i + 1)
  in
  go from

let slice s i j = String.sub s i (j - i)

let field header =
  let lines = String.split_on_char '\n' header in
  let lines = List.map (fun line ->
    let n = String.length line in
    if n = 0 || line.[n - 1] <> '\r' then bad ();
    String.sub line 0 (n - 1)) lines in
  let dispositions = List.filter_map (fun line ->
    match String.index_opt line ':' with
    | None -> bad ()
    | Some i when String.lowercase_ascii (slice line 0 i) = "content-disposition" ->
        Some (String.trim (slice line (i + 1) (String.length line)))
    | Some _ -> None) lines in
  match dispositions with
  | [ value ] ->
      (match List.map String.trim (String.split_on_char ';' value) with
      | "form-data" :: attrs ->
          (match List.filter_map (fun a ->
            if String.starts_with ~prefix:"name=\"" a && String.ends_with ~suffix:"\"" a then
              Some (slice a 6 (String.length a - 1)) else None) attrs with
          | [ name ] when List.mem name [ "name"; "problem"; "lang"; "source"; "file";
            "title"; "start"; "end"; "time"; "memory"; "statement"; "input"; "output" ] -> name
          | _ -> bad ())
      | _ -> bad ())
  | _ -> bad ()

let boundary content_type =
  match List.map String.trim (String.split_on_char ';' content_type) with
  | kind :: attrs when String.lowercase_ascii kind = "multipart/form-data" ->
      (match List.filter_map (fun attr ->
        if String.starts_with ~prefix:"boundary=" attr then Some (slice attr 9 (String.length attr)) else None) attrs with
      | [ b ] ->
          let b = if String.length b >= 2 && b.[0] = '"' && b.[String.length b - 1] = '"'
            then slice b 1 (String.length b - 1) else b in
          if String.length b = 0 || String.length b > 70 ||
             not (String.for_all (function 'a'..'z' | 'A'..'Z' | '0'..'9' | '-' | '_' | '\'' | '(' | ')' |
               '+' | ',' | '.' | '/' | ':' | '=' | '?' -> true | _ -> false) b) then bad ();
          b
      | _ -> bad ())
  | _ -> bad ()

let parse content_type body =
  let b = boundary content_type in
  let start = "--" ^ b ^ "\r\n" in
  let separator = "\r\n--" ^ b in
  if not (String.starts_with ~prefix:start body) then bad ();
  let rec delimiter i =
    let pos = try search body separator i with Not_found -> bad () in
    let after = pos + String.length separator in
    if after + 2 <= String.length body &&
       (slice body after (after + 2) = "--" || slice body after (after + 2) = "\r\n") then pos
    else delimiter (pos + 1)
  in
  let rec parts i acc =
    let h = try search body "\r\n\r\n" i with Not_found -> bad () in
    if h - i > 4096 then bad ();
    let name = field (slice body i h ^ "\r") in
    if List.mem_assoc name acc then bad ();
    let first = h + 4 in
    let last = delimiter first in
    let value = slice body first last in
    let next = last + String.length separator in
    if next + 2 > String.length body then bad ();
    if slice body next (next + 2) = "--" then begin
      let end_pos = next + 2 in
      if end_pos <> String.length body && slice body end_pos (String.length body) <> "\r\n" then bad ();
      List.rev ((name, value) :: acc)
    end else if slice body next (next + 2) = "\r\n" then parts (next + 2) ((name, value) :: acc)
    else bad ()
  in
  parts (String.length start) []

let read ?(limit = 131072) content_type length =
  let n = try int_of_string length with _ -> bad () in
  if n < 0 || n > limit then invalid_arg "request too large";
  set_binary_mode_in stdin true;
  let body = try really_input_string stdin n with End_of_file -> bad () in
  parse content_type body

let get fields name =
  match List.assoc_opt name fields with Some v -> v | None -> bad ()
