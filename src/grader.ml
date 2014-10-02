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

(** Returns true iff string pre occurs as a substring of s at position i *)
let occurs_at (s : string) (i : int) (pre : string) : bool =
  let rec loop j =
    if String.length pre = j then true
    else if String.length s = i + j then false
    else pre.[j] = s.[i + j] && loop (j + 1) in
  loop 0

let find_substring_from (s : string) (i : int) (sub : string) : int =
  let rec loop i =
    if String.length s <= i then raise @@ Invalid_argument "find_substring_from"
    else if occurs_at s i sub then i
    else loop (i + 1) in
  loop i

let process_file (path : string) : unit =
  let chan = open_in path in
  let nbytes = in_channel_length chan in
  let s = String.create nbytes in
  let manual_tag = "(* GRADE_MANUAL" in
  let auto_tag = "(* GRADE_THEOREM" in
  let term = "*)" in
  really_input chan s 0 nbytes;
  let rec loop i =
    if String.length s <= i then ()
    else if occurs_at s i manual_tag then
      let i = i + String.length manual_tag in
      let j = String.index_from s i ':' in
      let k = find_substring_from s i term in
      let n = int_of_string @@ String.trim @@ String.sub s i (j - i) in
      let m = String.trim @@ String.sub s (j + 1) (k - j - 1) in
      Printf.printf "Found manual %d with message \"%s\"\n" n m;
      loop (k + String.length term)
    else if occurs_at s i auto_tag then
      let i = i + String.length auto_tag in
      let j = String.index_from s i ':' in
      let k = find_substring_from s i term in
      let n = int_of_string @@ String.trim @@ String.sub s i (j - i) in
      let m = String.trim @@ String.sub s (j + 1) (k - j - 1) in
      Printf.printf "Found auto %d with message \"%s\"\n" n m;
      loop (k + String.length term)
    else loop (i + 1) in
  try
    loop 0;
    close_in chan
  with Invalid_argument _ -> Printf.printf "Error while reading file\n"; exit 1

let grade () : unit =
  let env = Array.append [|"SFGRADINGRESULT=" ^ o.result_file|] @@
    Unix.environment () in
  let proc = Unix.open_process_full coqcom env in
  let input, output, _ = proc in
  output_string output plugin_loader_com;
  flush output;
  match Unix.close_process_full proc with
  | Unix.WEXITED i -> Printf.printf "WEXITED %u\n" i
  | _ -> ()

let _ = process_file @@ Printf.sprintf "%s/%s.v" o.sf_path o.assignment
