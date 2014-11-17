type cfg = {
  cfg_sf_path : string option
}

type options = {
  sf_path : string;
  submission : string;
  result_dir : string
}

let (/) = Filename.concat

let cfg =
  let cfg_path = Sys.getenv "HOME" / ".sf-grader" in
  let pairs = Hashtbl.create 10 in
  let re = Str.regexp "\\([^ =]*\\) *= *\\([^ =]*\\)" in
  begin
    try
      let ic = open_in cfg_path in
      let rec read () =
        try
          let line = input_line ic in
          if Str.string_match re line 0 then
            let name = Str.matched_group 1 line in
            let value = Str.matched_group 2 line in
            Hashtbl.add pairs name value
          else
            Printf.printf "Just read line %s" line;
          read ()
        with End_of_file -> close_in ic in
      read ();
    with Sys_error _ ->
    (* File probably doesn't exist; simply leave option table empty *)
      ()
  end;
  let cfg_sf_path =
    try Some (Hashtbl.find pairs "sf-path")
    with Not_found -> None in
  { cfg_sf_path = cfg_sf_path }

let usage () : 'a =
  List.iter print_endline [
    "USAGE: grader [OPTIONS] SUBMISSION";
    "Grade submissions of Software Foundations exercises";
    "";
    "OPTIONS";
    "  --sf-path - The path to the Software Foundations sources";
    "  -o        - Where to output grading results (default: submissions)";
  ];
  exit 0

let read_options () : options =
  let sf_path = ref cfg.cfg_sf_path in
  let submission = ref None in
  let result_dir = ref None in
  let rec process args =
    begin match args with
    | "--help" :: _ -> usage ()
    | "--sf-path" :: path :: args ->
      sf_path := Some path;
      process args
    | "--sf-path" :: _ -> usage ()
    | "-o" :: path :: args ->
      result_dir := Some path;
      process args
    | "-o" :: _ -> usage ()
    | path :: args ->
      if !submission == None then
        (submission := Some path;
         process args)
      else usage ()
    | [] -> ()
    end in
  let args = Array.to_list Sys.argv in
  process (List.tl args);
  match !sf_path, !submission with
  | Some sf_path, Some submission ->
    { result_dir =
        begin match !result_dir with
        | Some rf -> rf
        | None -> "submissions"
        end;
      sf_path = sf_path; submission = submission }
  | _, _ -> usage ()

let o = read_options ()

let translate_file_name name =
  let file_format = Str.regexp "\\([^-]*\\)--\\([^_]*\\)_[^_]*_[^_]*_\\(.*\\)" in
  if not @@ Str.string_match file_format name 0 then
    failwith @@ Printf.sprintf "Don't know what to do with file %s" name;
  let last_name = Str.matched_group 1 name in
  let first_name = Str.matched_group 2 name in
  let file_name = Str.matched_group 3 name in
  let first_name =
    (* Strip "-late" prefix, if present *)
    if Str.string_match (Str.regexp "\\(.*\\)-late$") first_name 0 then begin
      Str.matched_group 1 first_name
    end
    else first_name in
  (Printf.sprintf "%s-%s" last_name first_name, file_name)

let ensure_dir_exists dir =
  if not @@ Sys.file_exists dir then Unix.mkdir dir 0o744

let noext path =
  try String.sub path 0 (String.rindex path '.')
  with
  | Not_found -> path
  | Invalid_argument _ -> path

let ext path =
  try
    let i = String.rindex path '.' + 1 in
    String.sub path i (String.length path - i)
  with
  | Not_found -> ""
  | Invalid_argument _ -> ""

let dirname path =
  try String.sub path 0 (String.rindex path '/')
  with
  | Not_found -> "."
  | Invalid_argument _ -> "."

let basename path =
  try
    let i = String.rindex path '/' + 1 in
    String.sub path i (String.length path - i)
  with
  | Not_found -> path
  | Invalid_argument _ -> path

let cp source dest =
  ignore @@ Sys.command @@ Printf.sprintf "cp %s %s" source dest

let plugin_loader_com = "Declare ML Module \"graderplugin\".\n"

let grade_sub path : unit =
  let assignment = noext @@ basename path in
  let tmp = "tmp" / "Submission.v" in
  cp path tmp;
  print_string path;
  let res = Sys.command @@ Printf.sprintf "coqc -I %s %s > %s.out 2>&1" o.sf_path tmp path in
  if res <> 0 then
    print_endline " compilation error"
  else begin
    print_endline " ok";
    let env = Array.append [|"SFGRADERRESULT=" ^ path ^ ".res";
                             "SFGRADERSFPATH=" ^ o.sf_path;
                             "SFGRADERASSIGNMENT=" ^ assignment |] @@
      Unix.environment () in
    let coqcom =
      Printf.sprintf
        "coqtop -I %s -I tmp -I src -require %s -require Submission > out 2>&1"
        o.sf_path assignment in
    let proc = Unix.open_process_full coqcom env in
    let input, output, _ = proc in
    output_string output plugin_loader_com;
    flush output
  end

let grade_subs path =
  let com = Printf.sprintf "unzip -qq %s -d %s" path "tmp" in
  ignore @@ Sys.command com;
  let files = Sys.readdir "tmp" in
  ensure_dir_exists o.result_dir;
  Array.iter (fun file ->
    let name, file' = translate_file_name file in
    let dir = o.result_dir / name in
    let path = dir / file' in
    ensure_dir_exists dir;
    cp ("tmp"/file) path;
    grade_sub path
  ) files

let _ =
  ensure_dir_exists "tmp";
  match ext o.submission with
  | "zip" -> grade_subs o.submission
  | "v" -> grade_sub o.submission
  | _ ->
    Printf.printf "Don't know what to do with file %s\n" o.submission;
    exit 1
