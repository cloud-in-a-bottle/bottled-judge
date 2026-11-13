type problem = { name : string; title : string; time : int; memory : int }
type t = { dir : string; title : string; token : string; start : int; stop : int; problems : problem list }

let days y m d =
  let y = if m <= 2 then y - 1 else y in
  let yoe = y mod 400 in
  let doy = ((153 * ((m + 9) mod 12)) + 2) / 5 + d - 1 in
  (y / 400 * 146097) + (yoe * 365) + (yoe / 4) - (yoe / 100) + doy - 719468

let time s = Scanf.sscanf s "%d-%d-%d %d:%d%!" (fun y m d h mi -> (days y m d * 86400) + (h * 3600) + (mi * 60))

let format_utc stamp =
  let t = Unix.gmtime (float stamp) in
  Printf.sprintf "%04d-%02d-%02d %02d:%02d" (t.tm_year + 1900) (t.tm_mon + 1) t.tm_mday t.tm_hour t.tm_min

let problem dir name =
  let h, _ = Kv.read (Filename.concat (Filename.concat dir name) "problem.txt") in
  { name; title = Kv.get h "title"; time = int_of_string (Kv.get h "time"); memory = int_of_string (Kv.get h "memory") }

let load dir =
  let h, _ = Kv.read (Filename.concat dir "contest.txt") in
  let token = Kv.get h "token" in
  if String.length token < 20 then failwith (dir ^ ": token too short");
  let ps = List.filter (fun p -> p.[0] <> '.' && Sys.is_directory (Filename.concat dir p))
    (List.sort compare (Array.to_list (Sys.readdir dir))) in
  { dir; title = Kv.get h "title"; token; start = time (Kv.get h "start"); stop = time (Kv.get h "end"); problems = List.map (problem dir) ps }

let same a b =
  let n = String.length a in
  let rec go i acc = if i = n then acc = 0 else go (i + 1) (acc lor (Char.code a.[i] lxor Char.code b.[i])) in
  n = String.length b && go 0 0

let find root token =
  List.find_opt (fun c -> same c.token token) (List.map (fun d -> load (Filename.concat root d)) (Array.to_list (Sys.readdir root)))
