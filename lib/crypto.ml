external random : int -> string = "bj_random"
external sha256 : string -> string = "bj_sha256"
external scrypt : string -> string -> string = "bj_scrypt"
external public_key : string -> string = "bj_ed25519_public"
external sign : string -> string -> string = "bj_ed25519_sign"
external verify : string -> string -> string -> bool = "bj_ed25519_verify"
external equal : string -> string -> bool = "bj_crypto_equal"

let hex value =
  let alphabet = "0123456789abcdef" in
  String.init (String.length value * 2) (fun i ->
    let byte = Char.code value.[i / 2] in
    alphabet.[if i mod 2 = 0 then byte lsr 4 else byte land 15])

let unhex value =
  let digit = function
    | '0'..'9' as c -> Char.code c - Char.code '0'
    | 'a'..'f' as c -> Char.code c - Char.code 'a' + 10
    | _ -> invalid_arg "Invalid hex" in
  if String.length value mod 2 <> 0 then invalid_arg "Invalid hex";
  String.init (String.length value / 2) (fun i ->
    Char.chr (digit value.[2*i] * 16 + digit value.[2*i+1]))

let key_init dir =
  let key = Filename.concat dir "private" in
  if not (Sys.file_exists key) then begin
    let temp = Filename.concat dir (".private." ^ hex (random 8)) in
    let fd = Unix.openfile temp [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_EXCL ] 0o600 in
    let out = Unix.out_channel_of_descr fd in
    output_string out (random 32);
    flush out;
    Unix.fsync fd;
    close_out out;
    Fun.protect ~finally:(fun () -> Unix.unlink temp) (fun () ->
      try Unix.link temp key with Unix.Unix_error (Unix.EEXIST, _, _) -> ())
  end;
  let value = File.read key in
  if String.length value <> 32 then failwith "Invalid instance key"
