let path = Filename.concat

let waiting folder contest read =
  Array.exists (fun name -> Sub.id_ok name && read (path folder name) = contest) (Sys.readdir folder)

let submissions data id = waiting (path data "pending") id (fun file -> (Sub.read file).contest)
let hacks data id = waiting (path (path data "hacks") "pending") id (fun file -> (Hack.read file).contest)

let applying data id =
  let folder = path (path data "hacks") "done" in
  Array.exists (fun name ->
    if not (Sub.id_ok name) || Sys.file_exists (path folder (name ^ ".applied")) then false else
    let hack = Hack.read (path folder name) in
    hack.contest = id && hack.result = Some Hack.Success) (Sys.readdir folder)

let can_submit (c : Contest.t) now = match c.schedule with
  | Some s -> c.phase = Contest.Running && now < s.starts + s.submissions
  | None -> false

let can_hack (c : Contest.t) now = match c.schedule, c.hacking_started with
  | Some s, Some started -> c.phase = Contest.Hacking && now < started + s.hacks
  | _ -> false

let advance data dir now =
  Contest.with_lock dir (fun () ->
    let rec step () =
      let c = Contest.load dir in
      match c.schedule with
      | None -> ()
      | Some s ->
          let next = match c.phase with
            | Contest.Ready when now >= s.starts -> Some Contest.Running
            | Contest.Running when now >= s.starts + s.submissions -> Some Contest.Stopped
            | Contest.Stopped when now >= s.starts + s.submissions + s.intermission &&
                not (submissions data c.id) -> Some Contest.Hacking
            | Contest.Hacking when not (can_hack c now) && not (hacks data c.id || applying data c.id) ->
                Some Contest.Finished
            | _ -> None in
          (match next with None -> () | Some phase -> Contest.set_phase dir phase now; step ()) in
    step ())
