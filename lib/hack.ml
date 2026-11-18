type outcome = Success | Failed | Invalid | Error

type t = {
  id : string; contest : string; problem : string; target : string; author : string;
  at : int; attempts : int; result : outcome option; input : string; expected : string option
}

let outcome_of_string = function
  | "SUCCESS" -> Success | "FAILED" -> Failed | "INVALID" -> Invalid | "ERROR" -> Error
  | value -> failwith ("Invalid hack outcome: " ^ value)

let string_of_outcome = function
  | Success -> "SUCCESS" | Failed -> "FAILED" | Invalid -> "INVALID" | Error -> "ERROR"

let read file =
  let fields, body = Kv.read file in
  let get = Kv.get fields in
  let result = Option.map outcome_of_string (List.assoc_opt "outcome" fields) in
  if result = Some Success && not (List.mem_assoc "answer-size" fields) then
    failwith "Successful hack needs an answer";
  let input, expected = match List.assoc_opt "answer-size" fields with
    | None -> body, None
    | Some value ->
        let size = int_of_string value in
        if size < 0 || size > String.length body then failwith "Invalid hack answer size";
        let length = String.length body - size in
        String.sub body 0 length, Some (String.sub body length size) in
  { id = Filename.basename file; contest = get "contest"; problem = get "problem";
    target = get "target"; author = get "author"; at = int_of_string (get "at");
    attempts = int_of_string (get "attempts"); result; input; expected }

let write file h =
  let fields = [ "contest", h.contest; "problem", h.problem; "target", h.target;
    "author", h.author; "at", string_of_int h.at; "attempts", string_of_int h.attempts ] in
  let result = match h.result with None -> [] | Some r -> [ "outcome", string_of_outcome r ] in
  let answer = match h.expected with None -> [] | Some s -> [ "answer-size", string_of_int (String.length s) ] in
  let body = h.input ^ Option.value ~default:"" h.expected in
  Kv.write file (fields @ result @ answer) body
