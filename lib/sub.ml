type result = { verdict : string; test : int; cpu : int }

type t = {
  id : string; contest : string; problem : string; name : string; lang : Lang.t;
  at : int; attempts : int; result : result option; source : string }

let id () =
  Random.self_init ();
  Printf.sprintf "%013d-%016Lx" (int_of_float (Unix.gettimeofday () *. 1000.)) (Random.bits64 ())

let name_ok s =
  let n = String.length s in
  n >= 1 && n <= 32 && s.[0] <> ' ' && s.[n - 1] <> ' ' && String.for_all (fun c -> c >= ' ' && c <= '~') s

let read p =
  let h, source = Kv.read p in
  let g = Kv.get h in
  let result = match List.assoc_opt "verdict" h with
    | Some verdict -> Some { verdict; test = int_of_string (g "test"); cpu = int_of_string (g "cpu") }
    | None -> None in
  { id = Filename.basename p; contest = g "contest"; problem = g "problem"; name = g "name"; lang = Lang.of_string (g "lang");
    at = int_of_string (g "at"); attempts = int_of_string (g "attempts"); result; source }

let write p s =
  let base = [ "contest", s.contest; "problem", s.problem; "name", s.name; "lang", Lang.to_string s.lang;
    "at", string_of_int s.at; "attempts", string_of_int s.attempts ] in
  let res = match s.result with
    | Some r -> [ "verdict", r.verdict; "test", string_of_int r.test; "cpu", string_of_int r.cpu ]
    | None -> [] in
  Kv.write p (List.append base res) s.source
