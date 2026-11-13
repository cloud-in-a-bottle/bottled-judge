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
  if s <> Contest.format_utc stamp then failwith "Use a real UTC date in YYYY-MM-DD HH:MM format";
  stamp

let title s =
  if String.length s = 0 || String.length s > 100 ||
     String.exists (fun c -> Char.code c < 32 || c = '\127') s then failwith "Invalid title"

let check_times start stop =
  if date start >= date stop then failwith "End must follow start"

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
    ignore (Test_set.numbers (path folder "tests"))) c.problems;
  c
