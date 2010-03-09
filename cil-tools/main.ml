open Cil

(* Put the contents of dir in a list *)
let ls dir = Array.to_list (Array.map (Filename.concat dir) (Sys.readdir dir))

(* Expand the directory tree by listing subdirectories *)
let rec expand_contents c =
   match c with
      | [] -> []
      | hdr :: cdr ->
         if Sys.is_directory hdr then
            List.concat [(expand_contents (ls hdr)); expand_contents cdr]
         else  hdr :: expand_contents cdr

(* Functions for extracting names *)
let lfname (host, offset) =
   match host with
   | Var v -> v.vname
   | Mem _ -> "mem!"

let fname e = 
   match e with
   | Lval l -> lfname l
   | e -> "<UNKNOWN>"

let type_str t =
   let d = d_typsig () (typeSig t) in
   Pretty.sprint 80 d

let print_loc l =
   print_string (l.file ^ ":");
   print_int l.line;
   print_newline ()

(* Make an lval into either a variable or None *)
let e_var e =
   match e with
   | Lval (host, offset) -> (match host with
     | Var v -> Some v
     | l -> None)
   | e -> None

(* List of all known allocation functions *)
let is_alloc_fn f = match f with
   | "kmem_cache_alloc"
   | "kmem_cache_alloc_notrace"
   | "kmalloc"
   | "kzalloc" -> true
   | f -> false


(* A callsite lists the dest variable, function variable, and file/line *)
type callsite = { dst_var : varinfo; fn_var : varinfo option; loc : location }

let cs_name (c : callsite) =
   match c with
   | {dst_var = _; fn_var = Some v; loc = _} -> v.vname
   | {dst_var = _; fn_var = None; loc = _} -> "<UNKNOWN>"

let cs_loc (c : callsite) = 
   match c with
   | {dst_var = _; fn_var = _; loc = l} -> l.file ^ ":" ^ (string_of_int l.line)


(* Find all allocation functions, build a list of the variables that the
   result is assigned to *)
class listInstVisitor = object(self)
   inherit nopCilVisitor
   val mutable var_ids = ([] : callsite list)
   method vinst (i : instr) =
      match i with
      | Call (lval, exp, args, location) ->
         if is_alloc_fn (fname exp) then (
            match lval with
            | Some (Var v, offset) ->
               var_ids <- {dst_var = v; fn_var = (e_var exp); loc = location} :: var_ids
            | lval -> print_loc location
         );
         DoChildren
      | Set _ -> DoChildren
      | Asm _ -> DoChildren

   method get_ids = var_ids
end


(* Given a list of variable ids, find all the ways those variables are cast *)
class useVisitor (var_ids : callsite list) = object(self)
   inherit nopCilVisitor
   method vinst (i : instr) =
      match i with
      | Set (lval, exp, loc) -> (
         match exp with
         | CastE (t, Lval (Var v, off)) -> 
           (try (
              let cs = List.find (fun x -> (x.dst_var.vid = v.vid)) var_ids in
              print_string ((cs_name cs) ^ " " ^ (cs_loc cs) ^ " ");
              print_endline (type_str t)
           ) with Not_found -> ());
           DoChildren
         | exp -> DoChildren
        )
      | i -> DoChildren
end

(* Visit all except allocation functions, run aforementioned two visitors to
 extract information on how allocation function results are cast *)
class fnVisitor = object(self)
   inherit nopCilVisitor
   method vfunc (f : fundec) =
      if not (is_alloc_fn f.svar.vname) then (
         let visit = (new listInstVisitor) in
         ignore (visitCilFunction (visit :> nopCilVisitor)  f);
         ignore (visitCilFunction (new useVisitor (visit#get_ids)) f);
         (* print_endline ("Visited " ^ f.svar.vname); *)
      ) else ((*print_endline ("Skipped " ^ f.svar.vname)*));
      SkipChildren
end

let () = 
   (* Get all files *)
   let dir = Sys.argv.(1) in
   let contents = ls dir in
   let contents = expand_contents contents in

   (* Parse each file *)
   let num_files = List.length contents in
   let fi = ref 0 in
   let parsed_files = List.map (fun filename ->
         incr fi;
         print_string (filename ^ " "); 
         print_int !fi; print_char '/'; print_int num_files;
         print_newline ();
         let file = (Frontc.parse filename) () in
         let () = Rmtmps.removeUnusedTemps file in
         file) contents
   in

   (* Merge all files into one *)
   let kernel = print_endline "MERGING"; Mergecil.merge parsed_files "test" in

   (* Dump the processed C *)
   let oc = open_out "blah.c" in
   dumpFile !printerForMaincil oc "blah" kernel;

   (* Run the analysis *)
   flush stderr;
   visitCilFileSameGlobals (new fnVisitor) kernel
