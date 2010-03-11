open Cil

(* Functions for extracting names *)
let lfname (host, offset) =
   match host with
   | Var v -> v.vname
   | Mem _ -> "mem!"

let fname e = 
   match e with
   | Lval l -> lfname l
   | e -> "<UNKNOWN>"

let typesig_str ts =
   let d = d_typsig () ts in
   Pretty.sprint 10000 d

let type_str t =
   typesig_str (typeSig t)

let loc_str l = 
   (l.file ^ ":" ^ (string_of_int l.line))

let print_loc l =
   print_endline (loc_str l)

