type outcome = Success | Failed | Invalid | Error

type t = {
  id : string; contest : string; problem : string; target : string; name : string;
  at : int; attempts : int; result : outcome option; input : string
}

let outcome_of_string = function
  | "SUCCESS" -> Success | "FAILED" -> Failed | "INVALID" -> Invalid | "ERROR" -> Error
  | value -> failwith ("Invalid hack outcome: " ^ value)

let string_of_outcome = function
  | Success -> "SUCCESS" | Failed -> "FAILED" | Invalid -> "INVALID" | Error -> "ERROR"

let read file =
  let fields, input = Kv.read file in
  let get = Kv.get fields in
  let result = Option.map outcome_of_string (List.assoc_opt "outcome" fields) in
  { id = Filename.basename file; contest = get "contest"; problem = get "problem";
    target = get "target"; name = get "name"; at = int_of_string (get "at");
    attempts = int_of_string (get "attempts"); result; input }

let write file h =
  let fields = [ "contest", h.contest; "problem", h.problem; "target", h.target;
    "name", h.name; "at", string_of_int h.at; "attempts", string_of_int h.attempts ] in
  let result = match h.result with None -> [] | Some r -> [ "outcome", string_of_outcome r ] in
  Kv.write file (List.append fields result) h.input
