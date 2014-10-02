type options = {
  sf_path : string;
  assignment : string;
  result_file : string
}

let usage () : 'a =
  Printf.printf "grader OPTIONS\n";
  exit 0

let read_options () : options =
  let sf_path = ref None in
  let assignment = ref None in
  let result_file = ref None in
  let rec process args =
    begin match args with
    | "--sf-path" :: path :: args ->
      sf_path := Some path;
      process args
    | "--sf-path" :: _ -> usage ()
    | "-o" :: path :: args ->
      result_file := Some path;
      process args
    | "-o" :: _ -> usage ()
    | path :: args ->
      if !assignment == None then
        (assignment := Some path;
         process args)
      else usage ()
    | [] -> ()
    end in
  let args = Array.to_list Sys.argv in
  process (List.tl args);
  match !sf_path, !assignment with
  | Some sf_path, Some assignment ->
    { result_file =
        begin match !result_file with
        | Some rf -> rf
        | None -> "result"
        end;
      sf_path = sf_path; assignment = assignment }
  | _, _ -> usage ()

let o = read_options ()

let plugin_loader_com = "Declare ML Module \"graderplugin\".\n"

let coqcom : string =
  Printf.sprintf "coqtop -I %s -I src -require %s -require Submission >> out 2>&1"
    o.sf_path o.assignment

let grade () : unit =
  let env = Array.append [|"SFGRADERRESULT=" ^ o.result_file;
                           "SFGRADERSFPATH=" ^ o.sf_path;
                           "SFGRADERASSIGNMENT=" ^ o.assignment |] @@
    Unix.environment () in
  let proc = Unix.open_process_full coqcom env in
  let input, output, _ = proc in
  output_string output plugin_loader_com;
  flush output;
  match Unix.close_process_full proc with
  | Unix.WEXITED i -> Printf.printf "WEXITED %u\n" i
  | _ -> ()

let _ = grade ()
