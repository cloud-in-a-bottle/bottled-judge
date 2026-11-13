let cap = 64 * 1024 * 1024

let sh s = Unix.system s = Unix.WEXITED 0

let rec reap n =
  if sh "pgrep -u run >/dev/null" then begin
    if n = 0 then failwith "leftovers";
    ignore (sh "setpriv --reuid=run --regid=run --clear-groups sh -c 'kill -9 -1'");
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

let run tl ml i o d cmd =
  let m = string_of_int (ml * 1024 * 1024) in
  let a = Array.append [| "/usr/bin/prlimit"; "--cpu=" ^ string_of_int (tl / 1000 + 1); "--data=" ^ m; "--stack=" ^ m;
    "--fsize=" ^ string_of_int cap; "--nproc=16"; "--nofile=64"; "--core=0"; "--";
    "setpriv"; "--reuid=run"; "--regid=run"; "--clear-groups"; "--no-new-privs"; "--";
    "unshare"; "--user"; "--net"; "--map-current-user"; "--" |] cmd in
  let t0 = Unix.times () and w0 = Unix.gettimeofday () in
  let pid = Unix.fork () in
  if pid = 0 then begin
    try
      ignore (Unix.setsid ());
      Unix.dup2 ~cloexec:false (Unix.openfile i [ O_RDONLY; O_CLOEXEC ] 0) Unix.stdin;
      Unix.dup2 ~cloexec:false (Unix.openfile o [ O_WRONLY; O_CREAT; O_TRUNC; O_CLOEXEC ] 0o644) Unix.stdout;
      Unix.dup2 ~cloexec:false (Unix.openfile "/dev/null" [ O_WRONLY; O_CLOEXEC ] 0) Unix.stderr;
      Unix.chdir d;
      Unix.execve a.(0) a [| "PATH=/usr/local/bin:/usr/bin:/bin"; "HOME=" ^ d; "LANG=C.UTF-8" |]
    with _ -> Unix._exit 127
  end;
  let killed, st = wait pid (w0 +. float (max (2 * tl) (tl + 1000)) /. 1000.) false in
  let t1 = Unix.times () in
  let c = int_of_float ((t1.tms_cutime +. t1.tms_cstime -. t0.tms_cutime -. t0.tms_cstime) *. 1000.) in
  let w = int_of_float ((Unix.gettimeofday () -. w0) *. 1000.) in
  reap 50;
  let sz = (Unix.stat o).st_size in
  let v = if killed || c > tl then "TLE" else if sz >= cap then "OLE" else if st <> WEXITED 0 then "RE" else "OK" in
  Printf.printf "%s cpu=%d wall=%d out=%d\n" v c w sz

let serve () =
  Sys.set_signal Sys.sigterm (Sys.Signal_handle (fun _ -> exit 0));
  let l = Unix.create_process "lighttpd" [| "lighttpd"; "-D"; "-f"; "/src/lighttpd.conf" |] Unix.stdin Unix.stdout Unix.stderr in
  let rec loop () = if fst (Unix.wait ()) <> l then loop () in
  loop ();
  exit 1

let () =
  match Array.to_list Sys.argv with
  | [ _ ] -> serve ()
  | _ :: "run" :: tl :: ml :: i :: o :: d :: "--" :: _ :: _ ->
      run (int_of_string tl) (int_of_string ml) i o d (Array.sub Sys.argv 8 (Array.length Sys.argv - 8))
  | _ -> prerr_endline "judge | judge run TL_MS ML_MB IN OUT DIR -- CMD..."; exit 2
