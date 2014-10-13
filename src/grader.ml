type options = {
  sf_path : string;
  submission : string;
  result_dir : string
}

let usage () : 'a =
  List.iter print_endline [
    "USAGE: grader [OPTION] SUBMISSION";
    "Grade submissions of Software Foundations exercises";
    "";
    "OPTIONS";
    "  --sf-path - The path to the Software Foundations sources";
    "  -o        - Where to output grading results (default: submissions)";
  ];
  exit 0

let read_options () : options =
  let sf_path = ref None in
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
  let i = String.index name '_' in
  let j = String.index_from name (i + 1) '_' in
  let j = String.index_from name (j + 1) '_' in
  (String.sub name 0 i,
   String.sub name (j + 1) (String.length name - j - 1))

let ensure_dir_exists dir =
  if not @@ Sys.file_exists dir then Unix.mkdir dir 0o744

let (/) base name =
  base ^ "/" ^ name

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
