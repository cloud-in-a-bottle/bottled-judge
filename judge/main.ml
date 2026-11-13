let cap = 64 * 1024 * 1024
let uid = 20000

type lim = { cpu : int; wall : int; mem : int; nproc : int; fsize : int }

let compiler : Bj.Lang.t -> string array = function
  | C -> [| "gcc"; "-std=c23"; "-O2"; "-o"; "main"; "main.c"; "-lm" |]
  | Cpp -> [| "g++"; "-std=c++23"; "-O2"; "-o"; "main"; "main.cpp" |]
  | Rust -> [| "rustc"; "--edition=2024"; "-O"; "-o"; "main"; "main.rs" |]
  | OCaml -> [| "ocamlopt"; "-o"; "main"; "main.ml" |]
  | Python -> [||]

let program : Bj.Lang.t -> string array = function Python -> [| "python3"; "main.py" |] | _ -> [| "./main" |]

let mine p = p.[0] >= '0' && p.[0] <= '9' && (try (Unix.stat ("/proc/" ^ p)).st_uid = uid with _ -> false)

let rec reap n =
  let l = List.filter mine (Array.to_list (Sys.readdir "/proc")) in
  if l <> [] then begin
    if n = 0 then failwith "leftovers";
    List.iter (fun p -> try Unix.kill (int_of_string p) Sys.sigkill with _ -> ()) l;
    Unix.sleepf 0.01;
    reap (n - 1)
  end

let rec wait pid dl k =
  match Unix.waitpid [ WNOHANG ] pid with
  | 0, _ when not k && Unix.gettimeofday () > dl ->
      (try Unix.kill (-pid) Sys.sigkill with _ -> ());
      wait pid dl true
  | 0, _ -> Unix.sleepf 0.005; wait pid dl k
  | _, s -> (k, s)

let sandbox l i o e d cmd =
  let a = Array.append [|
    "/usr/bin/prlimit";
    Printf.sprintf "--cpu=%d" (l.cpu / 1000 + 1);
    Printf.sprintf "--data=%d" (l.mem lsl 20);
    Printf.sprintf "--stack=%d" (l.mem lsl 20);
    Printf.sprintf "--fsize=%d" l.fsize;
    Printf.sprintf "--nproc=%d" l.nproc;
    "--nofile=256"; "--core=0"; "--";
    "/usr/bin/setpriv"; "--reuid=20000"; "--regid=20000"; "--clear-groups"; "--no-new-privs"; "--";
    "/usr/bin/unshare"; "--user"; "--net"; "--map-current-user"; "--" |] cmd in
  let t0 = Unix.times () and w0 = Unix.gettimeofday () in
  let pid = Unix.fork () in
  if pid = 0 then begin
    try
      ignore (Unix.setsid ());
      Unix.dup2 ~cloexec:false (Unix.openfile i [ O_RDONLY; O_CLOEXEC ] 0) Unix.stdin;
      Unix.dup2 ~cloexec:false (Unix.openfile o [ O_WRONLY; O_CREAT; O_TRUNC; O_CLOEXEC ] 0o644) Unix.stdout;
      Unix.dup2 ~cloexec:false (Unix.openfile e [ O_WRONLY; O_CREAT; O_TRUNC; O_CLOEXEC ] 0o644) Unix.stderr;
      Unix.chdir d;
      Unix.execve a.(0) a [| "PATH=/usr/local/bin:/usr/bin:/bin"; "HOME=" ^ d; "LANG=C.UTF-8" |]
    with _ -> Unix._exit 127
  end;
  let k, st = wait pid (w0 +. float l.wall /. 1000.) false in
  let t1 = Unix.times () in
  let c = int_of_float ((t1.tms_cutime +. t1.tms_cstime -. t0.tms_cutime -. t0.tms_cstime) *. 1000.) in
  let w = int_of_float ((Unix.gettimeofday () -. w0) *. 1000.) in
  reap 50;
  (k, st, c, w)

let regular p =
  try (Unix.lstat p).st_kind = Unix.S_REG with Unix.Unix_error (Unix.ENOENT, _, _) -> false

let execute tl ml i o d cmd =
  let k, st, c, w = sandbox { cpu = tl; wall = max (2 * tl) (tl + 1000); mem = ml; nproc = 16; fsize = cap } i o "/dev/null" d cmd in
  let sz = if regular o then (Unix.stat o).st_size else 0 in
  let v = if k || c > tl then "TLE" else if sz >= cap then "OLE" else if st <> WEXITED 0 || not (regular o) then "RE" else "OK" in
  (v, c, w, sz)

let run tl ml i o d cmd =
  let v, c, w, sz = execute tl ml i o d cmd in
  Printf.printf "%s cpu=%d wall=%d out=%d\n" v c w sz

let build x d =
  let cc = compiler x and e = Filename.concat d "ce" in
  if cc = [||] then (true, 0, 0, "")
  else begin
    let k, st, c, w = sandbox { cpu = 30000; wall = 30000; mem = 2048; nproc = 64; fsize = 2 * cap } "/dev/null" "/dev/null" e d cc in
    if not (regular e) then (false, c, w, "invalid compiler output")
    else begin
      let ic = open_in_bin e in
      let text = really_input_string ic (min 16384 (in_channel_length ic)) in
      close_in ic;
      (not k && st = WEXITED 0, c, w, text)
    end
  end

let compile x d =
  let ok, c, w, text = build x d in
  Printf.printf "%s cpu=%d wall=%d\n%s" (if ok then "OK" else "CE") c w text

let data () = Sys.getenv "OPENHOST_APP_DATA_DIR"
let temp () = Sys.getenv "OPENHOST_APP_TEMP_DIR"
let path a b = Filename.concat a b

let ensure p mode =
  if not (Sys.file_exists p) then Unix.mkdir p mode;
  if not (Sys.is_directory p) then failwith (p ^ " is not a directory")

let rec remove p =
  if (Unix.lstat p).st_kind = Unix.S_DIR then begin
    Array.iter (fun x -> remove (path p x)) (Sys.readdir p);
    Unix.rmdir p
  end else Unix.unlink p

let write p text =
  let tmp = path (Filename.dirname p) ("." ^ Filename.basename p) in
  let oc = open_out_bin tmp in
  output_string oc text;
  flush oc;
  Unix.fsync (Unix.descr_of_out_channel oc);
  close_out oc;
  Sys.rename tmp p

let prepare_sandbox () =
  let dir = path (temp ()) "sandbox" in
  if Sys.file_exists dir then remove dir;
  Unix.mkdir dir 0o700;
  Unix.chown dir uid uid;
  dir

let source_name : Bj.Lang.t -> string = function
  | C -> "main.c" | Cpp -> "main.cpp" | Rust -> "main.rs" | OCaml -> "main.ml" | Python -> "main.py"

let equal_files a b =
  let x = open_in_bin a and y = open_in_bin b in
  let one = Bytes.create 65536 and two = Bytes.create 65536 in
  let rec compare () =
    let n = input x one 0 (Bytes.length one) in
    let m = input y two 0 (Bytes.length two) in
    n = m && (n = 0 || (Bytes.sub one 0 n = Bytes.sub two 0 m && compare ()))
  in
  match compare () with
  | result -> close_in x; close_in y; result
  | exception ex -> close_in_noerr x; close_in_noerr y; raise ex

let tests dir =
  let entries = Array.to_list (Sys.readdir dir) in
  let inputs = List.filter (fun s -> Filename.check_suffix s ".in") entries in
  let numbers = List.map (fun s -> int_of_string (Filename.chop_suffix s ".in")) inputs in
  let numbers = List.sort compare numbers in
  if numbers = [] then failwith (dir ^ ": no tests");
  List.iteri (fun i n ->
    if n <> i + 1 || not (Sys.file_exists (path dir (string_of_int n ^ ".out"))) then
      failwith (dir ^ ": missing test pair")) numbers;
  List.iter (fun f ->
    if Filename.check_suffix f ".out" && not (List.mem f (List.map (fun n -> string_of_int n ^ ".out") numbers)) then
      failwith (dir ^ ": unmatched output")) entries;
  numbers

let judge_submission s =
  let contest_dir = path (path (data ()) "contests") s.Bj.Sub.contest in
  let contest = Bj.Contest.load contest_dir in
  let problem = List.find (fun p -> p.Bj.Contest.name = s.problem) contest.problems in
  let test_dir = path (path contest.dir problem.name) "tests" in
  let cases = tests test_dir in
  let dir = prepare_sandbox () in
  let source = path dir (source_name s.lang) in
  write source s.source;
  Unix.chown source uid uid;
  let ok, _, _, error = build s.lang dir in
  if not ok then ({ Bj.Sub.verdict = "CE"; test = 0; cpu = 0 }, Some error)
  else begin
    let rec check cpu = function
      | [] -> ({ Bj.Sub.verdict = "AC"; test = 0; cpu }, None)
      | n :: rest ->
          let input = path test_dir (string_of_int n ^ ".in") in
          let expected = path test_dir (string_of_int n ^ ".out") in
          let output = path dir "out" in
          (try remove output with Unix.Unix_error (Unix.ENOENT, _, _) -> ());
          let verdict, time, _, _ = execute problem.time problem.memory input output dir (program s.lang) in
          let verdict = if verdict = "OK" then (if equal_files output expected then "AC" else "WA") else verdict in
          let cpu = cpu + time in
          if verdict = "AC" then check cpu rest else ({ Bj.Sub.verdict; test = n; cpu }, None)
    in
    check 0 cases
  end

let pending () = path (data ()) "pending"
let done_dir () = path (data ()) "done"

let access p uid gid mode =
  let s = Unix.stat p in
  if s.st_uid <> uid || s.st_gid <> gid then Unix.chown p uid gid;
  if s.st_perm <> mode then Unix.chmod p mode

let secure_contests () =
  let root = path (data ()) "contests" in
  access root 0 1000 0o750;
  Array.iter (fun name ->
    let dir = path root name in
    if Sys.is_directory dir then begin
      access dir 0 1000 0o750;
      Array.iter (fun name ->
        let p = path dir name in
        if Sys.is_directory p then begin
          access p 0 1000 0o750;
          let tests = path p "tests" in
          if Sys.file_exists tests then access tests 0 0 0o700
        end) (Sys.readdir dir)
    end) (Sys.readdir root)

let finish id s result error =
  let target = path (done_dir ()) id in
  (match error with Some text -> write (target ^ ".ce") text
  | None -> (try Unix.unlink (target ^ ".ce") with Unix.Unix_error (Unix.ENOENT, _, _) -> ()));
  Bj.Sub.write target { s with result = Some result };
  Unix.unlink (path (pending ()) id);
  let dir = path (temp ()) "sandbox" in
  if Sys.file_exists dir then remove dir

let process id =
  let file = path (pending ()) id in
  if Sys.file_exists (path (done_dir ()) id) then Unix.unlink file
  else begin
    let s = Bj.Sub.read file in
    if s.attempts >= 3 then finish id s { Bj.Sub.verdict = "IE"; test = 0; cpu = 0 } None
    else begin
      let s = { s with attempts = s.attempts + 1 } in
      Bj.Sub.write file s;
      let result, error =
        try judge_submission s with ex ->
          prerr_endline (Printexc.to_string ex);
          ({ Bj.Sub.verdict = "IE"; test = 0; cpu = 0 }, None)
      in
      finish id s result error
    end
  end

let first_pending () =
  let files = Array.to_list (Sys.readdir (pending ())) in
  let files = List.filter (fun f -> String.length f = 30 && f.[13] = '-') files in
  match List.sort compare files with [] -> None | id :: _ -> Some id

let worker () =
  ensure (path (data ()) "contests") 0o755;
  ensure (pending ()) 0o700;
  access (pending ()) 1000 1000 0o700;
  ensure (done_dir ()) 0o755;
  access (done_dir ()) 0 1000 0o750;
  ensure (temp ()) 0o755;
  let rec loop () =
    secure_contests ();
    (match first_pending () with Some id -> process id | None -> Unix.sleepf 0.5);
    loop ()
  in
  loop ()

let serve () =
  Sys.set_signal Sys.sigterm (Sys.Signal_handle (fun _ -> (try Unix.kill (-1) Sys.sigterm with _ -> ()); exit 0));
  let l = Unix.create_process "lighttpd" [| "lighttpd"; "-D"; "-f"; "/src/lighttpd.conf" |] Unix.stdin Unix.stdout Unix.stderr in
  let j = Unix.fork () in
  if j = 0 then begin
    Sys.set_signal Sys.sigterm Sys.Signal_default;
    try worker () with ex -> prerr_endline (Printexc.to_string ex); exit 1
  end;
  let rec loop () =
    let p, _ = Unix.wait () in
    if p <> l && p <> j then loop ()
  in
  loop ();
  (try Unix.kill l Sys.sigterm with _ -> ());
  (try Unix.kill j Sys.sigterm with _ -> ());
  (try reap 50 with _ -> ());
  exit 1

let () =
  match Array.to_list Sys.argv with
  | [ _ ] -> serve ()
  | [ _; "compile"; x; d ] -> compile (Bj.Lang.of_string x) d
  | _ :: "run" :: tl :: ml :: i :: o :: d :: "--" :: _ :: _ ->
      run (int_of_string tl) (int_of_string ml) i o d (Array.sub Sys.argv 8 (Array.length Sys.argv - 8))
  | _ -> prerr_endline "judge | judge compile LANG DIR | judge run TL_MS ML_MB IN OUT DIR -- CMD..."; exit 2
