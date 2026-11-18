type problem = { name : string; title : string; time : int; memory : int }
type phase = Ready | Running | Stopped | Hacking | Finished
type t = { dir : string; title : string; token : string; phase : phase; problems : problem list }

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
  let token = Kv.get h "token" in
  if String.length token <> 32 || not (String.for_all (function
    | '0'..'9' | 'a'..'f' -> true | _ -> false) token) then failwith (dir ^ ": invalid token");
  let ps = List.filter (fun p -> p.[0] <> '.' && Sys.is_directory (Filename.concat dir p))
    (List.sort compare (Array.to_list (Sys.readdir dir))) in
  { dir; title = Kv.get h "title"; token; phase = phase_of_string (Kv.get h "phase"); problems = List.map (problem dir) ps }

let set_phase dir phase =
  let file = Filename.concat dir "contest.txt" in
  let h, body = Kv.read file in
  let h = List.filter (fun (name, _) -> name <> "phase") h in
  Kv.write file (List.append h [ "phase", string_of_phase phase ]) body

let with_lock dir f =
  let fd = Unix.openfile (Filename.concat dir ".phase.lock") [ Unix.O_CREAT; Unix.O_RDWR ] 0o600 in
  Fun.protect ~finally:(fun () -> Unix.close fd) (fun () ->
    Unix.lockf fd Unix.F_LOCK 0;
    f ())

let same a b =
  let n = String.length a in
  let rec go i acc = if i = n then acc = 0 else go (i + 1) (acc lor (Char.code a.[i] lxor Char.code b.[i])) in
  n = String.length b && go 0 0

let find root token =
  List.find_opt (fun c -> same c.token token) (List.map (fun d -> load (Filename.concat root d)) (Array.to_list (Sys.readdir root)))
