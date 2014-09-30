open Coqlib
open Global
open Libnames
open Pp
open Printer
open Util

let find_reference (path : string list) (id : string) : global_reference option =
  try
    Some (find_reference "" path id)
  with
  | Anomaly (_,_) -> None

let () = Mltop.add_known_plugin (fun () ->
  Printf.printf "Grader plugin successfully loaded\n\n";
  let r = find_reference ["Coq";"Lists";"List"] "app" in
  match r with
  | Some r ->
    pp (pr_global r);
    pp (brk (0,0));
    pp (pr_constr (type_of_global r))
  | None   -> Printf.printf "Couldn't find anything"
) "grader"
