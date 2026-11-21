type problem = { name : string; title : string; time : int; memory : int }
type phase = Ready | Running | Stopped | Hacking | Finished
type schedule = { starts : int; submissions : int; intermission : int; hacks : int }
type t = { dir : string; title : string; id : string; phase : phase; problems : problem list;
  schedule : schedule option; started : int option; hacking_started : int option; finished : int option }

let utc timestamp =
  let t = Unix.gmtime (float_of_int timestamp) in
  Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02dZ" (t.tm_year + 1900) (t.tm_mon + 1)
    t.tm_mday t.tm_hour t.tm_min t.tm_sec

let timestamp value =
  if String.length value <> 20 || value.[4] <> '-' || value.[7] <> '-' || value.[10] <> 'T' ||
     value.[13] <> ':' || value.[16] <> ':' || value.[19] <> 'Z' then invalid_arg "Invalid UTC start";
  let number offset length = int_of_string (String.sub value offset length) in
  let year = number 0 4 and month = number 5 2 and day = number 8 2 in
  let hour = number 11 2 and minute = number 14 2 and second = number 17 2 in
  if year < 2000 || year > 2100 || month < 1 || month > 12 || day < 1 || day > 31 ||
     hour > 23 || minute > 59 || second > 59 then invalid_arg "Invalid UTC start";
  let y = year - (if month <= 2 then 1 else 0) in
  let era = y / 400 and m = month + (if month > 2 then -3 else 9) in
  let yoe = y - era * 400 in
  let days = era * 146097 + yoe * 365 + yoe / 4 - yoe / 100 +
    (153 * m + 2) / 5 + day - 1 - 719468 in
  let result = ((days * 24 + hour) * 60 + minute) * 60 + second in
  if utc result <> value then invalid_arg "Invalid UTC start";
  result

let schedule_fields s = [ "starts", string_of_int s.starts;
  "submissions", string_of_int s.submissions; "intermission", string_of_int s.intermission;
  "hacks", string_of_int s.hacks ]

let id_ok id =
  let n = String.length id in
  n > 0 && n <= 15 && id.[0] <> '0' &&
  String.for_all (function '0'..'9' -> true | _ -> false) id && int_of_string_opt id <> None

let phase_of_string = function
  | "ready" -> Ready
  | "running" -> Running
  | "stopped" -> Stopped
  | "hacking" -> Hacking
  | "finished" -> Finished
  | value -> failwith ("Invalid contest phase: " ^ value)

let string_of_phase = function
  | Ready -> "ready" | Running -> "running" | Stopped -> "stopped"
  | Hacking -> "hacking" | Finished -> "finished"

let problem dir name =
  let h, _ = Kv.read (Filename.concat (Filename.concat dir name) "problem.txt") in
  { name; title = Kv.get h "title"; time = int_of_string (Kv.get h "time"); memory = int_of_string (Kv.get h "memory") }

let load dir =
  let h, _ = Kv.read (Filename.concat dir "contest.txt") in
  let id = Filename.basename dir in
  if not (id_ok id) then failwith (dir ^ ": invalid contest ID");
  let ps = List.filter (fun p -> p.[0] <> '.' && Sys.is_directory (Filename.concat dir p))
    (List.sort compare (Array.to_list (Sys.readdir dir))) in
  let optional name = Option.map int_of_string (List.assoc_opt name h) in
  let schedule = match List.assoc_opt "starts" h with None -> None | Some start ->
    Some { starts = int_of_string start; submissions = int_of_string (Kv.get h "submissions");
      intermission = int_of_string (Kv.get h "intermission"); hacks = int_of_string (Kv.get h "hacks") } in
  { dir; title = Kv.get h "title"; id; phase = phase_of_string (Kv.get h "phase"); problems = List.map (problem dir) ps;
    schedule; started = optional "started"; hacking_started = optional "hacking_started";
    finished = optional "finished" }

let update dir fields =
  let file = Filename.concat dir "contest.txt" in
  let h, body = Kv.read file in
  let h = List.filter (fun (name, _) -> not (List.mem_assoc name fields)) h in
  Kv.write file (h @ fields) body

let set_schedule dir schedule =
  update dir (schedule_fields schedule)

let set_phase dir phase now =
  let time = match phase with
    | Running -> [ "started", string_of_int now ]
    | Hacking -> [ "hacking_started", string_of_int now ]
    | Finished -> [ "finished", string_of_int now ]
    | Ready | Stopped -> [] in
  update dir (("phase", string_of_phase phase) :: time)

let with_lock dir f =
  let old = Unix.umask 0o007 in
  let fd = try Unix.openfile (Filename.concat dir ".phase.lock") [ Unix.O_CREAT; Unix.O_RDWR ] 0o660
    with ex -> ignore (Unix.umask old); raise ex in
  ignore (Unix.umask old);
  Fun.protect ~finally:(fun () -> Unix.close fd) (fun () ->
    Unix.lockf fd Unix.F_LOCK 0;
    f ())

let same a b =
  let n = String.length a in
  let rec go i acc = if i = n then acc = 0 else go (i + 1) (acc lor (Char.code a.[i] lxor Char.code b.[i])) in
  n = String.length b && go 0 0

let find root id =
  let dir = Filename.concat root id in
  if id_ok id && Sys.file_exists (Filename.concat dir "contest.txt") then Some (load dir) else None
