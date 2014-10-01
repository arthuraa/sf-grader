let (|>) x f = x f
let (@@) f x = f x

type options = {
  sf_path : string;
  assignment : string;
  result_file : string
}

let default_options : options = {
  sf_path = "~/src/sf";
  assignment = "MoreCoq";
  result_file = "result"
}

let plugin_loader_com = "Declare ML Module \"graderplugin\".\n"

let coqcom (o : options) : string =
  Printf.sprintf "coqtop -I %s -I src -require %s -require Submission >> out 2>&1"
    o.sf_path o.assignment

let grade (o : options) : unit =
  let env = Array.append [|"SFGRADINGRESULT=" ^ o.result_file|] @@
    Unix.environment () in
  let proc = Unix.open_process_full (coqcom o) env in
  let input, output, _ = proc in
  output_string output plugin_loader_com;
  flush output;
  match Unix.close_process_full proc with
  | Unix.WEXITED i -> Printf.printf "WEXITED %u\n" i
  | _ -> ()

let _ = grade default_options
