open Reductionops
open Coqlib
open Environ
open Global
open Libnames
open Pp
open Printer
open Term
open Util

let sf_path = Sys.getenv "SFGRADERSFPATH"
let assignment = Sys.getenv "SFGRADERASSIGNMENT"
let result_file = Sys.getenv "SFGRADERRESULT"

let has_no_assumptions (id : global_reference) : bool =
  constr_of_global id |>
  Assumptions.assumptions ~add_opaque:false Names.full_transparent_state |>
  Assumptions.ContextObjectMap.is_empty

let is_conv (t1 : constr) (t2 : constr) : bool =
  is_conv empty_env Evd.empty t1 t2

let ofind_reference (path : string list) (id : string) : global_reference option =
  try
    Some (find_reference "" path id)
  with
  | Anomaly (_,_) -> None

let compare_defs (file : string) (id : string) : bool =
  let orig = find_reference "Couldn't find original definition in SF file"
    [file] id in
  let sub = ofind_reference ["Submission"] id in
  match sub with
  | Some sub ->
    pp (pr_constr (type_of_global orig));
    pp (brk (0,0));
    pp (pr_constr (type_of_global orig));
    pp (brk (0,0));
    is_conv (type_of_global sub) (type_of_global orig) &&
    has_no_assumptions sub
  | None -> false

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

let () =
  Mltop.add_known_plugin (fun () -> ()) "grader";
  Printf.printf "Grader plugin successfully loaded\n\n";
  (* let out = open_out result in
     let f   = Format.formatter_of_out_channel out in
     let result = if compare_defs "MoreCoq" "filter_exercise" then "true" else "false" in
     Format.fprintf f "Worked = %s@." result;
     close_out out *)
  process_file @@ Printf.sprintf "%s/%s.v" sf_path assignment
