open Reductionops
open Coqlib
open Environ
open Global
open Libnames
open Pp
open Printer
open Term
open Util

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

let () =
  Mltop.add_known_plugin (fun () -> ()) "grader";
  Printf.printf "Grader plugin successfully loaded\n\n";
  let out = open_out @@ Sys.getenv "SFGRADINGRESULT" in
  let f   = Format.formatter_of_out_channel out in
  let result = if compare_defs "MoreCoq" "filter_exercise" then "true" else "false" in
  Format.fprintf f "Worked = %s@." result;
  close_out out
