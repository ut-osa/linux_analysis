(*
 * linux_analysis project
 * File name: tools.ml
 * 
 * Description: 
 * 
 * Operating Systems & Architecture Group
 * University of Texas at Austin - Department of Computer Sciences
 * Copyright 2010, 2011. All Rights Reserved.
 * See LICENSE file for license terms.
 *)

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

let noAttrTypeSig t = typeSigWithAttrs (fun x -> []) t

let typesig_str ts =
   let d = d_typsig () ts in
   Pretty.sprint 10000 d

let type_str t =
   typesig_str (typeSig t)

let loc_str l = 
   (l.file ^ ":" ^ (string_of_int l.line))

let print_loc l =
   print_endline (loc_str l)

let gen_src_dir = Sys.argv.(2)
let output_file name = open_out (Filename.concat gen_src_dir name)

let is_directory d =
   (Unix.stat d).Unix.st_kind = Unix.S_DIR

let ctype_str t =
   Pretty.sprint 80 (!printerForMaincil#pType None () t)

let progress_map id f l =
   let num = List.length l in
   let fi = ref 0 in
   List.map (fun item -> 
         incr fi;
         prerr_string (id ^ " ");
         prerr_int !fi; prerr_char '/'; prerr_int num; prerr_char ' ';
         let res = f item in (prerr_newline (); res)) l

let progress_iter id f l =
   let num = List.length l in
   let fi = ref 0 in
   List.iter (fun item -> 
         incr fi;
         prerr_string (id ^ " ");
         prerr_int !fi; prerr_char '/'; prerr_int num; prerr_char ' ';
         let res = f item in (prerr_newline (); res)) l
