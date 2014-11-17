open Reductionops
open Coqlib
open Environ
open Global
open Libnames
open Pp
open Printer
open Term
open Util

type id = {
  mods : string list;
  name : string
}

let read_id s =
  let dot = Str.regexp "\\." in
  let names = Str.split dot s in
  let rec loop names =
    match names with
    | [] -> failwith @@ Format.sprintf "Error while parsing identifier %s" s
    | [name] -> { mods = []; name = name }
    | mod_name :: names -> let id = loop names in
                           { mods = mod_name :: id.mods;
                             name = id.name } in
  loop names

let pp_id (f : Format.formatter) (id : id) =
  Format.pp_print_list
    ~pp_sep:(fun f () -> Format.pp_print_string f ".")
    Format.pp_print_string f
    (id.mods @ [id.name])

type meth =
| CheckType of id
| RunTest of id * id
| Manual of string

type item = {
  meth : meth;
  points : int
}

let is_auto = function
  | CheckType _ -> true
  | RunTest (_,_) -> true
  | _ -> false

let is_manual i = not @@ is_auto i

type exercise = {
  ex_name : string;
  ex_advanced : bool;
  ex_items : item list
}

let ex_points f ex =
  List.fold_left (fun acc i ->
    if f i then acc + i.points
    else acc
  ) 0 ex.ex_items

let ex_auto_points ex =
  ex_points (fun i -> is_auto i.meth) ex

let ex_manual_points ex =
  ex_points (fun i -> is_manual i.meth) ex

let sf_path = Sys.getenv "SFGRADERSFPATH"
let assignment = Sys.getenv "SFGRADERASSIGNMENT"
let result_file = Sys.getenv "SFGRADERRESULT"

let has_no_assumptions (id : global_reference) : bool =
  constr_of_global id |>
  Assumptions.assumptions ~add_opaque:false Names.full_transparent_state |>
  Assumptions.ContextObjectMap.is_empty

let is_conv (t1 : constr) (t2 : constr) : bool =
  is_conv empty_env Evd.empty t1 t2

let find_reference (message : string) (path : string list) (id : id) : global_reference =
  find_reference message (path @ id.mods) id.name

let ofind_reference (path : string list) (id : id) : global_reference option =
  try
    Some (find_reference "" path id)
  with
  | Anomaly (_,_) -> None

(** Compare the type of a definition against the one it should have,
    given in the SF sources *)
let check_type (file : string) (id : id) : bool =
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
    torig = tnew && has_no_assumptions sub
  | None -> false

let run_test (file : string) (test_fun : id) (id : id) : bool =
  Format.printf "Should run %a on %a@." pp_id test_fun pp_id id;
  false

let read_file (path : string) : string =
  let chan = open_in path in
  let nbytes = in_channel_length chan in
  let s = String.create nbytes in
  really_input chan s 0 nbytes;
  close_in chan;
  s

let process_file (file : string) : exercise list =
  let tag_re = Str.regexp "^(\\* \\([^\n]*\\) \\*)" in
  let ex_re = Str.regexp "\\(EX[^ ]*\\) +(\\(.*\\))" in
  let manual_re = Str.regexp "GRADE_MANUAL \\([0-9]+\\): \\(.*\\)" in
  let check_type_re = Str.regexp "GRADE_THEOREM \\([0-9]+\\): \\(.*\\)" in
  let run_test_re = Str.regexp "GRADE_TEST \\([0-9]+\\): \\([^ ]*\\) +\\([^ ]*\\)" in

  let find_next i =
    try
      let _ = Str.search_forward tag_re file i in
      let j = Str.match_end () in
      let tag = Str.matched_group 1 file in
      if Str.string_match ex_re tag 0 then
        let name = Str.matched_group 2 tag in
        let advanced = String.contains (Str.matched_group 1 tag) 'A' in
        `New_ex (name, advanced, j)
      else `Tag (tag, j)
    with Not_found -> `End in

  let rec read i name advanced exs items =
    match find_next i with
    | `New_ex (name', advanced', i) ->
      let ex = { ex_name = name;
                 ex_advanced = advanced;
                 ex_items = List.rev items } in
      read i name' advanced' (ex :: exs) []
    | `Tag (tag, i) ->
      if Str.string_match manual_re tag 0 then
        let points = int_of_string @@ Str.matched_group 1 tag in
        let comment = Str.matched_group 2 tag in
        let meth = Manual comment in
        read i name advanced exs ({ meth = meth; points = points } :: items)
      else if Str.string_match check_type_re tag 0 then
        let points = int_of_string @@ Str.matched_group 1 tag in
        let id = read_id @@ Str.matched_group 2 tag in
        let meth = CheckType id in
        read i name advanced exs ({ meth = meth; points = points } :: items)
      else if Str.string_match run_test_re tag 0 then
        let points = int_of_string @@ Str.matched_group 1 tag in
        let test_fun = Str.matched_group 2 tag in
        let id = Str.matched_group 3 tag in
        let meth = RunTest (read_id test_fun, read_id id) in
        read i name advanced exs ({ meth = meth; points = points } :: items)
      else
        read i name advanced exs items
    | `End ->
      let ex = { ex_name = name;
                 ex_advanced = advanced;
                 ex_items = List.rev items } in
      List.rev (ex :: exs) in

  let rec find_first_ex i =
    match find_next i with
    | `New_ex (name, advanced, i) ->
      read i name advanced [] []
    | `Tag (_, i) -> find_first_ex i
    | `End -> [] in

  find_first_ex 0

let () =
  Mltop.add_known_plugin (fun () -> ()) "grader";
  Format.printf "Grader plugin successfully loaded@.@.";
  let out = open_out result_file in
  let f   = Format.formatter_of_out_channel out in
  let exs = process_file @@ read_file @@ Printf.sprintf "%s/%s.v" sf_path assignment in
  let ex_auto_grades ex =
    List.fold_left (fun acc i ->
      match i.meth with
      | CheckType id ->
        if check_type assignment id then acc + i.points
        else acc
      | RunTest (test_fun, id) ->
        if run_test assignment test_fun id then acc + i.points
        else acc
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
    if List.exists (fun i -> is_manual i.meth) ex.ex_items then begin
      let cat = if ex.ex_advanced then "A" else "S" in
      Format.fprintf f "%s (%s)@." ex.ex_name cat;
      List.iter (fun i ->
        match i.meth with
        | Manual com -> Format.fprintf f "  %s - %d@." com i.points
        | _ -> ()
      ) ex.ex_items
    end
  ) exs;
  let max_std_manual =
    List.fold_left (fun acc ex ->
      if ex.ex_advanced then acc
      else acc + ex_manual_points ex
    ) 0 exs in
  let max_adv_manual =
    List.fold_left (fun acc ex ->
      if ex.ex_advanced then acc + ex_manual_points ex
      else acc
    ) 0 exs in
  Format.fprintf f "@.Standard: -/%d@.Advanced: -/%d@." max_std_manual max_adv_manual
