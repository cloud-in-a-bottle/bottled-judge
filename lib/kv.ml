let read p =
  let ic = open_in_bin p in
  let rec go acc =
    match input_line ic with
    | "" -> List.rev acc
    | l -> go (Scanf.sscanf l "%[^:]: %[^\n]" (fun k v -> (k, v)) :: acc)
    | exception End_of_file -> List.rev acc
  in
  let h = go [] in
  let b = really_input_string ic (in_channel_length ic - pos_in ic) in
  close_in ic;
  (h, b)

let get h k = try List.assoc k h with Not_found -> failwith (k ^ " missing")

let write p h b =
  List.iter (fun (k, v) -> if String.contains k ':' || String.contains k '\n' || String.contains v '\n' then invalid_arg k) h;
  let t = Filename.concat (Filename.dirname p) ("." ^ Filename.basename p) in
  let oc = open_out_gen [ Open_wronly; Open_creat; Open_trunc; Open_binary ] 0o644 t in
  List.iter (fun (k, v) -> Printf.fprintf oc "%s: %s\n" k v) h;
  output_char oc '\n';
  output_string oc b;
  flush oc;
  Unix.fsync (Unix.descr_of_out_channel oc);
  close_out oc;
  Sys.rename t p
