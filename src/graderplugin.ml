open Reductionops
open Coqlib
open Environ
open Global
open Libnames
open Pp
open Printer
open Term
open Util

(** Coq identifiers. { mods = ["Mod1"; ...; "Modn"]; name = "name" }
    represents identifier "Mod1...Modn.name" *)
type id = {
  mods : string list;
  name : string
}

(** Read an identifier from its string representation *)
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

(** Formatter for identifiers *)
let pp_id (f : Format.formatter) (id : id) =
  Format.pp_print_list
    ~pp_sep:(fun f () -> Format.pp_print_string f ".")
    Format.pp_print_string f
    (id.mods @ [id.name])

(** Grading methods. Each file is annotated with grading directives
    that specify how each exercise should be graded.

    - [CheckType (ex, axioms)] checks whether the submission contains
    a solution for [ex], and that it has the same type as the one in
    the SF sources. It also checks whether the solution was admitted
    or used any axioms, ignoring any axioms present in the [axioms]
    list.

    - [RunTest (test_fun, ex)] runs Coq function [test_fun], which
    should be present in the original SF file, passing it [ex] as an
    argument. If the test returns [true], we count points for this
    exercise.

    - [Manual comment] doesn't do any checks, but reminds the grader
    that something should be graded manually. The [comment] is just
    used to remind the grader what to look for when grading that
    exercise, and is printed in the result file. *)

type meth =
| CheckType of id * id list
| RunTest of id * id
| Manual of string

let pp_meth (f : Format.formatter) (m : meth) =
  match m with
  | CheckType (ex, axioms) ->
    Format.fprintf f "CheckType {ex = %a, axioms = [%a]}"
      pp_id ex
      (Format.pp_print_list
         ~pp_sep:(fun f () -> Format.pp_print_string f ", ")
         pp_id)
      axioms
  | RunTest (test_fun, ex) ->
    Format.fprintf f "RunTest {test = %a, ex = %a}"
      pp_id test_fun pp_id ex
  | Manual comment -> Format.fprintf f "Manual \"%s\"" comment

(** Each item to be graded consists of a grading method [meth] and a
    number of [points] saying how much that item is worth. *)
type item = {
  meth : meth;
  points : int
}

let pp_item (f : Format.formatter) (i : item) =
  Format.fprintf f "%a (%d points)"
    pp_meth i.meth i.points

let is_auto = function
  | CheckType (_,_) -> true
  | RunTest (_,_) -> true
  | _ -> false

let is_manual i = not @@ is_auto i

type exercise = {
  ex_name : string;
  ex_advanced : bool;
  ex_items : item list
}

let pp_exercise (f : Format.formatter) (ex : exercise) =
  Format.fprintf f "%s (%s) %a"
    ex.ex_name
    (if ex.ex_advanced then "A" else "S")
    (Format.pp_print_list
       ~pp_sep:(fun f () -> Format.pp_print_string f ", ") pp_item)
    ex.ex_items

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

let is_conv (t1 : constr) (t2 : constr) : bool =
  let env, gamma = Lemmas.get_current_context () in
  is_conv gamma env t1 t2

let has_no_assumptions (id : global_reference) (allowed : Refset.t) : bool =
  let assumptions =
    constr_of_global id |>
    Assumptions.assumptions ~add_opaque:false Names.full_transparent_state in
  Assumptions.ContextObjectMap.for_all (fun obj ty ->
    match obj with
    | Assumptions.Axiom c ->
      Refset.exists (fun c' -> is_conv ty (type_of_global c')) allowed
    | _ -> false
  ) assumptions

let find_reference (message : string) (path : string list) (id : id) : global_reference =
  find_reference message (path @ id.mods) id.name

let ofind_reference (path : string list) (id : id) : global_reference option =
  try Some (find_reference "" path id)
  with Anomaly (_,_) -> None

let find_reference_sf_or_local (assignment : string) (id : id) : global_reference option =
  try Some (find_reference "" [assignment] id)
  with Anomaly (_,_) -> ofind_reference ["Submission"] id

(** Compare the type of a definition against the one it should have,
    given in the SF sources *)
let check_type (file : string) (id : id) (allowed : Refset.t) : bool =
  let orig = find_reference "Couldn't find original definition in SF file"
    [file] id in
  let sub = ofind_reference ["Submission"] id in
  match sub with
  | Some sub ->
    let torig =
      Str.global_replace (Str.regexp_string "\n") "" @@
      Str.global_replace (Str.regexp " +") " " @@
      Str.global_replace (Str.regexp_string @@ file ^ ".") "" @@
        string_of_ppcmds @@ pr_constr @@ type_of_global orig in
    let tnew =
      Str.global_replace (Str.regexp_string "\n") "" @@
      Str.global_replace (Str.regexp " +") " " @@
        string_of_ppcmds @@ pr_constr @@ type_of_global sub in
    torig = tnew && has_no_assumptions sub allowed
  | None -> false

let run_test (file : string) (test_fun : id) (id : id) : bool =
  let test_fun = find_reference "Couldn't find test function in SF file"
    [file] test_fun in
  let def = ofind_reference ["Submission"] id in
  match def with
  | Some def ->
    is_conv (mkApp (constr_of_global test_fun, [|constr_of_global def|]))
      (constr_of_global glob_true)
  | None -> false

let read_file (path : string) : string =
  let chan = open_in path in
  let nbytes = in_channel_length chan in
  let s = String.create nbytes in
  really_input chan s 0 nbytes;
  close_in chan;
  s

(** Reads the string given as its argument, collecting all exercises
    that are found there. *)
let process_file (file : string) : exercise list =
  let tag_re = Str.regexp "^(\\*\\*? *\\([^\n]*\\) *\\*)" in
  let ex_begin_re = Str.regexp ".*\\(EX[^ ]*\\) +(\\(.*\\))" in
  let ex_end_re = Str.regexp "\\[\\]" in
  let manual_re = Str.regexp "GRADE_MANUAL \\([0-9]+\\) *: \\(.*\\)" in
  let check_type_re = Str.regexp "GRADE_THEOREM \\([0-9]+\\) *: *\\([^ ]*\\)\\(.*\\)" in
  let run_test_re = Str.regexp "GRADE_TEST \\([0-9]+\\) *: \\([^ ]*\\) +\\([^ ]*\\)" in

  (* Rudimentary lexer. Starts parsing file at position 0, collecting
     everything that could be an exercise or grading directive *)
  let read_tag =
    let cur_pos = ref 0 in fun () ->
    try
      let _ = Str.search_forward tag_re file (!cur_pos) in
      cur_pos := Str.match_end ();
      let tag = Str.matched_group 1 file in
      if Str.string_match ex_begin_re tag 0 then
        let name = Str.matched_group 2 tag in
        let advanced = String.contains (Str.matched_group 1 tag) 'A' in
        `ExerciseBegin (name, advanced)
      else if Str.string_match manual_re tag 0 then
        let points = int_of_string @@ Str.matched_group 1 tag in
        let comment = Str.matched_group 2 tag in
        let meth = Manual comment in
        `Item ({ meth = meth; points = points })
      else if Str.string_match check_type_re tag 0 then
        let points = int_of_string @@ Str.matched_group 1 tag in
        let id = Str.matched_group 2 tag in
        let rest = Str.matched_group 3 tag in
        let allowed =
          if Str.string_match (Str.regexp " *(\\([^)]*\\)) *") rest 0 then
            List.map read_id @@ Str.split (Str.regexp " +") @@
              Str.matched_group 1 rest
          else [] in
        let meth = CheckType (read_id id, allowed) in
        `Item ({ meth = meth; points = points })
      else if Str.string_match run_test_re tag 0 then
        let points = int_of_string @@ Str.matched_group 1 tag in
        let test_fun = Str.matched_group 2 tag in
        let id = Str.matched_group 3 tag in
        let meth = RunTest (read_id test_fun, read_id id) in
        `Item ({ meth = meth; points = points })
      else if Str.string_match ex_end_re tag 0 then
        `ExerciseEnd
      else
        `Other tag
    with Not_found -> `End in

  let rec read_exercise name advanced exs items =
    let finish_ex () =
      { ex_name = name;
        ex_advanced = advanced;
        ex_items = List.rev items } in

    match read_tag () with
    | `ExerciseBegin (name', advanced') ->
      Format.printf "Warning: unterminated exercise %s ran into %s@." name name';
      read_exercise name' advanced' (finish_ex () :: exs) []
    | `Item item ->
      read_exercise name advanced exs (item :: items)
    | `ExerciseEnd ->
      read_exercises (finish_ex () :: exs)
    | `Other _ -> read_exercise name advanced exs items
    | `End ->
      Format.printf "Warning: found EOF before finishing exercise %s@." name;
      List.rev (finish_ex () :: exs)

  and read_exercises (exs : exercise list) =
    match read_tag () with
    | `ExerciseBegin (name, advanced) ->
      read_exercise name advanced exs []
    | `End -> List.rev exs
    | `Item _ | `ExerciseEnd | `Other _ ->
      read_exercises exs in

  read_exercises []

let () =
  Mltop.add_known_plugin (fun () -> ()) "grader";
  Format.printf "Grader plugin successfully loaded@.@.";
  let out = open_out result_file in
  let f   = Format.formatter_of_out_channel out in
  let exs = process_file @@ read_file @@ Printf.sprintf "%s/%s.v" sf_path assignment in
  let ex_auto_grades ex =
    List.fold_left (fun acc i ->
      match i.meth with
      | CheckType (id, allowed) ->
        let allowed =
          List.fold_left (fun acc id ->
            match find_reference_sf_or_local assignment id with
            | Some gr -> Refset.add gr acc
            | None -> acc
          ) Refset.empty allowed in
        if check_type assignment id allowed then acc + i.points
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

(* Local Variables: *)
(* compile-command: "make -k -C .." *)
(* End: *)
