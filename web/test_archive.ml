let path = Filename.concat

let contents zip =
  let entries = Zip.entries zip in
  if List.length entries > 401 then failwith "At most 200 test pairs are allowed";
  let seen = Hashtbl.create 401 in
  let size = ref 0 in
  let files = List.filter (fun (entry : Zip.entry) ->
    let name = entry.filename in
    if Hashtbl.mem seen name then failwith "Duplicate ZIP entry";
    Hashtbl.add seen name ();
    if entry.is_directory then begin
      if name <> "tests/" then failwith "The ZIP must contain only tests/";
      false
    end else begin
      if not (String.starts_with ~prefix:"tests/" name) then failwith "The ZIP must contain only tests/";
      let file = String.sub name 6 (String.length name - 6) in
      let extension = if Filename.check_suffix file ".in" then ".in"
        else if Filename.check_suffix file ".out" then ".out" else failwith "Expected numbered .in/.out files" in
      let number = Filename.chop_suffix file extension in
      let n = match int_of_string_opt number with Some n -> n | None -> 0 in
      if n < 1 || n > 200 || string_of_int n <> number then failwith "Test numbers must be 1 through 200";
      if entry.uncompressed_size < 0 || entry.uncompressed_size > 2097152 || entry.compressed_size < 0 then
        failwith "Each test file must be at most 2 MiB";
      size := !size + entry.uncompressed_size;
      if !size > 33554432 then failwith "Tests must total at most 32 MiB";
      true
    end) entries in
  let count = List.length files / 2 in
  if count = 0 || List.length files <> count * 2 then failwith "Each input needs an output";
  let rec check n =
    if n <= count then begin
      if not (Hashtbl.mem seen ("tests/" ^ string_of_int n ^ ".in") &&
              Hashtbl.mem seen ("tests/" ^ string_of_int n ^ ".out")) then
        failwith "Test pairs must be numbered from 1 without gaps";
      check (n + 1)
    end
  in
  check 1;
  files

let replace folder archive_bytes =
  if String.length archive_bytes = 0 then failwith "Choose a ZIP file";
  let id = Bj.Sub.id () in
  let original = path folder "tests" in
  let staging = path folder (".tests-" ^ id) in
  let backup = path folder (".old-tests-" ^ id) in
  let archive = path original (".archive-" ^ id) in
  Unix.mkdir staging 0o700;
  try
    Bj.Admin_data.write archive archive_bytes 0o600;
    let zip = Zip.open_in archive in
    Fun.protect ~finally:(fun () -> Zip.close_in zip) (fun () ->
      let entries = contents zip in
      List.iter (fun (entry : Zip.entry) ->
        let name = String.sub entry.filename 6 (String.length entry.filename - 6) in
        let bytes = Zip.read_entry zip entry in
        if String.length bytes <> entry.uncompressed_size || String.length bytes > 2097152 then
          failwith "Invalid test size";
        Bj.Admin_data.write (path staging name) bytes 0o600) entries);
    ignore (Bj.Admin_data.tests staging);
    Unix.unlink archive;
    Sys.rename original backup;
    (try Sys.rename staging original with ex -> Sys.rename backup original; raise ex);
    Bj.Admin_data.remove backup
  with ex ->
    if Sys.file_exists backup && not (Sys.file_exists original) then Sys.rename backup original;
    if Sys.file_exists staging then Bj.Admin_data.remove staging;
    if Sys.file_exists archive then Unix.unlink archive;
    raise ex
