open Reductionops
open Coqlib
open Environ
open Global
open Libnames
open Pp
open Printer
open Term
open Util

type item =
| Auto of string * int
| Manual of string * int

let is_auto = function
  | Auto (_,_) -> true
  | _ -> false

let is_manual i = not @@ is_auto i

type exercise = {
  ex_name : string;
  ex_advanced : bool;
  ex_items : item list
}

let ex_auto_points ex =
  List.fold_left (fun acc i ->
    match i with
    | Auto (_, n) -> acc + n
    | _ -> acc
  ) 0 ex.ex_items

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
    let torig =
      Str.global_replace (Str.regexp_string "\n") "" @@
      Str.global_replace (Str.regexp_string @@ file ^ ".") "" @@
        string_of_ppcmds @@ pr_constr @@ type_of_global orig in
    let tnew =
      Str.global_replace (Str.regexp_string "\n") "" @@
        string_of_ppcmds @@ pr_constr @@ type_of_global sub in
    Format.printf "%s@.%s@." torig tnew;
    if not @@ has_no_assumptions sub then Format.printf "I have assumptions!@.";
    Format.printf "@.";
    torig = tnew && has_no_assumptions sub
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
    if String.length s <= i then raise @@ Not_found
    else if occurs_at s i sub then i
    else loop (i + 1) in
  loop i

let read_file (path : string) : string =
  let chan = open_in path in
  let nbytes = in_channel_length chan in
  let s = String.create nbytes in
  really_input chan s 0 nbytes;
  close_in chan;
  s

let process_file (file : string) : exercise list =
  let ex_tag = "(* EX" in
  let manual_tag = "(* GRADE_MANUAL" in
  let auto_tag = "(* GRADE_THEOREM" in
  let term = "*)" in
  let rec read_exs i exs =
    if String.length file <= i then List.rev exs else
    let i = i + String.length ex_tag + 1 in
    let advanced = file.[i] = 'A' in
    let i = String.index_from file i ' ' in
    let j = find_substring_from file i term in
    let name = String.trim @@ String.sub file i (j - i) in
    let rec read_items i items =
      if String.length file <= i || occurs_at file i ex_tag then
        let ex = { ex_name = name;
                   ex_advanced = advanced;
                   ex_items = List.rev items } in
        read_exs i (ex :: exs)
      else if occurs_at file i manual_tag then
        let i = i + String.length manual_tag in
        let j = String.index_from file i ':' in
        let k = find_substring_from file i term in
        let n = int_of_string @@ String.trim @@ String.sub file i (j - i) in
        let m = String.trim @@ String.sub file (j + 1) (k - j - 1) in
        read_items (k + String.length term) (Manual (m, n) :: items)
      else if occurs_at file i auto_tag then
        let i = i + String.length auto_tag in
        let j = String.index_from file i ':' in
        let k = find_substring_from file i term in
        let n = int_of_string @@ String.trim @@ String.sub file i (j - i) in
        let m = String.trim @@ String.sub file (j + 1) (k - j - 1) in
        read_items (k + String.length term) (Auto (m, n) :: items)
      else read_items (i + 1) items in
    read_items (j + String.length term) [] in
  let start = try Some (find_substring_from file 0 ex_tag) with Not_found -> None in
  match start with
  | Some start ->
    (try read_exs start [] with _ -> Format.printf "Error while reading file@."; exit 1)
  | None -> []

let () =
  Mltop.add_known_plugin (fun () -> ()) "grader";
  Format.printf "Grader plugin successfully loaded@.@.";
  let out = open_out result_file in
  let f   = Format.formatter_of_out_channel out in
  let exs = process_file @@ read_file @@ Printf.sprintf "%s/%s.v" sf_path assignment in
  let ex_auto_grades ex =
    List.fold_left (fun acc i ->
      match i with
      | Auto (id, n) ->
        Format.printf "Item %s is worth %d@." id n;
        if compare_defs assignment id then
          (Format.printf "Convertible@."; acc + n)
        else (Format.printf "Not convertible@."; acc)
      | _ -> acc)
      0 ex.ex_items in
  let auto_grades = List.map ex_auto_grades exs in
  Format.fprintf f "Automatic Grades@.";
  Format.fprintf f "================@.@.";
  List.iter2 (fun ex grade ->
    let max = ex_auto_points ex in
    let cat = if ex.ex_advanced then "A" else "S" in
    if max <> 0 then Format.fprintf f "%s (%s) %d/%d@." ex.ex_name cat grade max
  ) exs auto_grades;
  let max_std =
    List.fold_left (fun acc ex ->
      if ex.ex_advanced then acc
      else acc + ex_auto_points ex
    ) 0 exs in
  let total_std =
    List.fold_left2 (fun acc ex grade ->
      if ex.ex_advanced then acc
      else acc + grade
    ) 0 exs auto_grades in
  let max_adv =
    List.fold_left (fun acc ex ->
      if ex.ex_advanced then acc + ex_auto_points ex
      else acc
    ) 0 exs in
  let total_adv =
    List.fold_left2 (fun acc ex grade ->
      if ex.ex_advanced then acc + grade
      else acc
    ) 0 exs auto_grades in
  Format.fprintf f "@.Standard: %d/%d@.Advanced: %d/%d@.@.@." total_std max_std total_adv max_adv;
  Format.fprintf f "Manual Grades@.";
  Format.fprintf f "=============@.@.";
  List.iter (fun ex ->
    if List.exists is_manual ex.ex_items then begin
      let cat = if ex.ex_advanced then "A" else "S" in
      Format.fprintf f "%s (%s)@." ex.ex_name cat;
      List.iter (fun i ->
        match i with
        | Manual (com, n) -> Format.fprintf f "  %s - %d@." com n
        | _ -> ()
      ) ex.ex_items
    end
  ) exs
