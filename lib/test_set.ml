let numbers dir =
  if (Unix.lstat dir).st_kind <> Unix.S_DIR then failwith "Missing tests directory";
  let files = List.filter (fun name -> name.[0] <> '.') (Array.to_list (Sys.readdir dir)) in
  let inputs = List.filter (fun name -> Filename.check_suffix name ".in") files in
  if inputs = [] || List.length files <> 2 * List.length inputs then
    failwith "Each problem needs paired tests";
  let numbers = List.sort compare (List.map (fun name ->
    let text = Filename.chop_suffix name ".in" in
    let n = int_of_string text in
    if n < 1 || string_of_int n <> text then failwith "Invalid test number";
    n) inputs) in
  List.iteri (fun index n ->
    if n <> index + 1 then failwith "Test numbers must start at 1 with no gaps";
    let file ext = Filename.concat dir (string_of_int n ^ ext) in
    if (Unix.lstat (file ".in")).st_kind <> Unix.S_REG ||
       (Unix.lstat (file ".out")).st_kind <> Unix.S_REG then failwith "Invalid test files") numbers;
  numbers
