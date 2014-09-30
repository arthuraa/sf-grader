open Coqlib

let () = Mltop.add_known_plugin (fun () ->
  Printf.printf "Grader plugin successfully loaded\n\n";
  let r = find_reference "Couldn't find it..." ["Coq";"Lists"] "List" in
  Printf.printf "Did it"
) "grader"
