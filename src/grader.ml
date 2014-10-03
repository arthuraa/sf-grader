type options = {
  sf_path : string;
  submissions : string;
  result_file : string
}

let usage () : 'a =
  List.iter print_endline [
    "USAGE: grader [OPTION] SUBMISSIONS";
    "Grade submissions of Software Foundations exercises";
    "";
    "OPTIONS";
    "  --sf-path - The path to the Software Foundations sources";
    "  -o        - Where to output grading results (default: results)";
  ];
  exit 0

let read_options () : options =
  let sf_path = ref None in
  let submissions = ref None in
  let result_file = ref None in
  let rec process args =
    begin match args with
    | "--help" :: _ -> usage ()
    | "--sf-path" :: path :: args ->
      sf_path := Some path;
      process args
    | "--sf-path" :: _ -> usage ()
    | "-o" :: path :: args ->
      result_file := Some path;
      process args
    | "-o" :: _ -> usage ()
    | path :: args ->
      if !submissions == None then
        (submissions := Some path;
         process args)
      else usage ()
    | [] -> ()
    end in
  let args = Array.to_list Sys.argv in
  process (List.tl args);
  match !sf_path, !submissions with
  | Some sf_path, Some submissions ->
    { result_file =
        begin match !result_file with
        | Some rf -> rf
        | None -> "result"
        end;
      sf_path = sf_path; submissions = submissions }
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

let cp source dest =
  ignore @@ Sys.command @@ Printf.sprintf "cp %s %s" source dest

let plugin_loader_com = "Declare ML Module \"graderplugin\".\n"

let grade_sub path file : unit =
  let assignment = String.sub file 0 (String.length file - 2) in
  let tmp = "tmp" / "Submission.v" in
  cp (path/file) tmp;
  print_string (path/file);
  let res = Sys.command @@ Printf.sprintf "coqc -I %s %s > %s.out 2>&1" o.sf_path tmp (path/file) in
  if res <> 0 then
    print_endline " compilation error"
  else begin
    print_endline " ok";
    let env = Array.append [|"SFGRADERRESULT=" ^ (path/file) ^ ".res";
                             "SFGRADERSFPATH=" ^ o.sf_path;
                             "SFGRADERASSIGNMENT=" ^ assignment |] @@
      Unix.environment () in
    let coqcom =
      Printf.sprintf
        "coqtop -I %s -I tmp -I src -require %s -require Submission >> out 2>&1"
        o.sf_path assignment in
    let proc = Unix.open_process_full coqcom env in
    let input, output, _ = proc in
    output_string output plugin_loader_com;
    flush output;
    match Unix.close_process_full proc with
    | Unix.WEXITED i -> Printf.printf "WEXITED %u\n" i
    | _ -> ()
  end

let copy_subs () =
  let com = Printf.sprintf "unzip -qq %s -d %s" "submissions.zip" "tmp" in
  ignore @@ Sys.command com;
  let files = Sys.readdir "tmp" in
  ensure_dir_exists "submissions";
  Array.iter (fun file ->
    let name, file' = translate_file_name file in
    let dir = "submissions" / name in
    ensure_dir_exists dir;
    cp ("tmp"/file) (dir/file');
    grade_sub dir file'
  ) files

let _ = copy_subs ()
