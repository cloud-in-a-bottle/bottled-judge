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

let run tl ml i o d cmd =
  let k, st, c, w = sandbox { cpu = tl; wall = max (2 * tl) (tl + 1000); mem = ml; nproc = 16; fsize = cap } i o "/dev/null" d cmd in
  let sz = (Unix.stat o).st_size in
  let v = if k || c > tl then "TLE" else if sz >= cap then "OLE" else if st <> WEXITED 0 then "RE" else "OK" in
  Printf.printf "%s cpu=%d wall=%d out=%d\n" v c w sz

let compile x d =
  let cc = compiler x and e = d ^ "/ce" in
  if cc = [||] then print_endline "OK cpu=0 wall=0"
  else begin
    let k, st, c, w = sandbox { cpu = 30000; wall = 30000; mem = 2048; nproc = 64; fsize = 2 * cap } "/dev/null" "/dev/null" e d cc in
    Printf.printf "%s cpu=%d wall=%d\n" (if k || st <> WEXITED 0 then "CE" else "OK") c w;
    let ic = open_in_bin e in
    print_string (really_input_string ic (min 16384 (in_channel_length ic)));
    close_in ic
  end

let serve () =
  Sys.set_signal Sys.sigterm (Sys.Signal_handle (fun _ -> exit 0));
  let l = Unix.create_process "lighttpd" [| "lighttpd"; "-D"; "-f"; "/src/lighttpd.conf" |] Unix.stdin Unix.stdout Unix.stderr in
  let rec loop () = if fst (Unix.wait ()) <> l then loop () in
  loop ();
  exit 1

let () =
  match Array.to_list Sys.argv with
  | [ _ ] -> serve ()
  | [ _; "compile"; x; d ] -> compile (Bj.Lang.of_string x) d
  | _ :: "run" :: tl :: ml :: i :: o :: d :: "--" :: _ :: _ ->
      run (int_of_string tl) (int_of_string ml) i o d (Array.sub Sys.argv 8 (Array.length Sys.argv - 8))
  | _ -> prerr_endline "judge | judge compile LANG DIR | judge run TL_MS ML_MB IN OUT DIR -- CMD..."; exit 2
