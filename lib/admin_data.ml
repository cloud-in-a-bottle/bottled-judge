let path = Filename.concat
let drafts data = path data "drafts"
let contests data = path data "contests"
let draft data id = path (drafts data) id
let published data id = path (contests data) id

let regular p = (Unix.lstat p).st_kind = Unix.S_REG
let directory p = (Unix.lstat p).st_kind = Unix.S_DIR

let rec remove p =
  if directory p then begin
    Array.iter (fun name -> remove (path p name)) (Sys.readdir p);
    Unix.rmdir p
  end else Unix.unlink p

let write p contents mode =
  let temp = path (Filename.dirname p) ("." ^ Filename.basename p ^ "." ^ Sub.id ()) in
  let oc = open_out_gen [ Open_wronly; Open_creat; Open_excl; Open_binary ] mode temp in
  output_string oc contents;
  flush oc;
  Unix.fsync (Unix.descr_of_out_channel oc);
  close_out oc;
  Sys.rename temp p

let date s =
  let stamp = Contest.time s in
  let d = Unix.gmtime (float stamp) in
  let actual = Printf.sprintf "%04d-%02d-%02d %02d:%02d"
    (d.tm_year + 1900) (d.tm_mon + 1) d.tm_mday d.tm_hour d.tm_min in
  if s <> actual then failwith "Use a real UTC date in YYYY-MM-DD HH:MM format";
  stamp

let title s =
  if String.length s = 0 || String.length s > 100 ||
     String.exists (fun c -> Char.code c < 32 || c = '\127') s then failwith "Invalid title"

let check_times start stop =
  if date start >= date stop then failwith "End must follow start"

let tests p =
  if not (directory p) then failwith "Missing tests directory";
  let files = Array.to_list (Sys.readdir p) in
  let visible = List.filter (fun name -> name.[0] <> '.') files in
  let inputs = List.filter (fun name -> Filename.check_suffix name ".in") visible in
  if inputs = [] || List.length visible <> List.length inputs * 2 then failwith "Each problem needs paired tests";
  let numbers = List.sort compare (List.map (fun name ->
    let number = Filename.chop_suffix name ".in" in
    let n = int_of_string number in
    if n < 1 || string_of_int n <> number then failwith "Invalid test number";
    n) inputs) in
  List.iteri (fun index n ->
    if n <> index + 1 then failwith "Test numbers must start at 1 with no gaps";
    let input = path p (string_of_int n ^ ".in") in
    let output = path p (string_of_int n ^ ".out") in
    if not (regular input && regular output) then failwith "Invalid test files") numbers;
  List.length numbers

let validate data id =
  let dir = draft data id in
  if not (directory dir) || not (regular (path dir "contest.txt")) then failwith "Invalid draft";
  let header, _ = Kv.read (path dir "contest.txt") in
  let start = Kv.get header "start" and stop = Kv.get header "end" in
  check_times start stop;
  if date start <= int_of_float (Unix.gettimeofday ()) then failwith "Start must be in the future";
  title (Kv.get header "title");
  let c = Contest.load dir in
  if String.length c.token <> 32 || not (String.for_all (function
    | '0'..'9' | 'a'..'f' -> true | _ -> false) c.token) then failwith "Invalid contest link";
  if c.problems = [] then failwith "Add a problem first";
  if Sys.file_exists (published data id) then failwith "Contest already exists";
  if Contest.find (contests data) c.token <> None then failwith "Contest link already exists";
  List.iter (fun p ->
    let folder = path dir p.Contest.name in
    title p.title;
    if p.time < 100 || p.time > 10000 || p.memory < 32 || p.memory > 1024 then
      failwith "Invalid problem limits";
    if not (directory folder && regular (path folder "problem.txt") &&
      regular (path folder "statement.html")) then failwith "Invalid problem files";
    let files = Array.to_list (Sys.readdir folder) in
    if List.sort compare files <> [ "problem.txt"; "statement.html"; "tests" ] then
      failwith "Unexpected problem files";
    ignore (tests (path folder "tests"))) c.problems;
  c

let ownership data id =
  let root = draft data id in
  let visible p uid gid mode =
    if not (regular p) then failwith "Invalid published file";
    Unix.chown p uid gid;
    Unix.chmod p mode in
  let dir p uid gid mode =
    if not (directory p) then failwith "Invalid published directory";
    Unix.chown p uid gid;
    Unix.chmod p mode in
  visible (path root "contest.txt") 0 1000 0o644;
  Array.iter (fun name ->
    let folder = path root name in
    if name.[0] = '.' && name <> ".publish" && name <> ".error" then remove folder
    else if directory folder then begin
      visible (path folder "problem.txt") 0 1000 0o644;
      visible (path folder "statement.html") 0 1000 0o644;
      let cases = path folder "tests" in
      Array.iter (fun file ->
        let p = path cases file in
        if file.[0] = '.' then Unix.unlink p else visible p 0 0 0o600) (Sys.readdir cases);
      dir cases 0 0 0o700;
      dir folder 0 1000 0o750
    end) (Sys.readdir root);
  Unix.unlink (path root ".publish");
  (try Unix.unlink (path root ".error") with Unix.Unix_error (Unix.ENOENT, _, _) -> ());
  dir root 0 1000 0o750;
  Sys.rename root (published data id)
