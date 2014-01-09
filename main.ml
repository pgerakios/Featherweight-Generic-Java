(* Module Main: The main program.  Deals with processing the command
   line, reading files, building and connecting lexers and parsers, etc. 
   
   For most experiments with the implementation, it should not be
   necessary to change this file.
*)

open Format
open Support.Pervasive
open Support.Error
open Syntax
open Core

let searchpath = ref [""]
let argDefs = [
      ("-I",
      Arg.String (fun f -> searchpath := f::!searchpath),
      "Append a directory to the search path");
      ("-d", Arg.Unit (fun () -> setDebug true), "Print stack trace");

      ("-u", Arg.Unit (fun () -> Type_safety.setUnsafe true), "Evaluate without type checking")
]
       

let parseArgs () =
  let inFile = ref (None : string option) in
  Arg.parse argDefs
     (fun s ->
       match !inFile with
         Some(_) -> err "You must specify exactly one input file"
       | None -> inFile := Some(s))
     "";
  match !inFile with
      None -> err "You must specify an input file"
    | Some(s) -> s

let openfile infile = 
  let rec trynext l = match l with
        [] -> err ("Could not find " ^ infile)
      | (d::rest) -> 
          let name = if d = "" then infile else (d ^ "/" ^ infile) in
          try open_in name
            with Sys_error m -> trynext rest
  in trynext !searchpath

let parseFile inFile =
  let pi = openfile inFile
  in let lexbuf = Lexer.create inFile pi
  in let result =
    try 
       Parser.toplevel Lexer.main lexbuf emptyNames
    with Parsing.Parse_error -> 
      error (Lexer.info lexbuf) "Parse error\n"
in
  Parsing.clear_parser(); close_in pi; result

let alreadyImported = ref ([] : string list)

let rec process_command (decls,terms) = 
   List.fold_left (fun round tm -> 
      prc Magenta "--------BEGIN COMMAND [%d] -----------\n" round;
      let prEx x = 
         onDebug(fun _ -> Printexc.print_backtrace stderr;
          pr "%s" ("Caught exception: "  ^ (Printexc.to_string x) ^ "\n")
         );
      in
      begin try Type_safety.runSafety decls tm
      with 
          Type_safety.ViolProgress(s) as x -> 
             prEx x;
             prc Red "%s" s 
        | Type_safety.ViolPreservation(s) as x -> 
              prEx x;
              prc Red "%s" s
        | Typecheck.TyError(tye) as x -> 
              prEx x;
              prc Red "Type error:\n %a" Typecheck.tye2p tye 
        | Substitution.SubstException sexn as x -> 
              prEx x;
              prc Red "Substitution error:\n %s" 
              (Substitution.string_of_sexn sexn)
      end;
      prc Magenta "\n--------- END COMMAND [%d] -----------\n" round;
      round + 1
   ) 0 terms;
   ()


let process_file f =
  alreadyImported := f :: !alreadyImported;
  let cmd = parseFile f in
  pr "Parsing stage ok\n";
  let cmds = [cmd] in
  let g cmd =  
    open_hvbox 0;
    process_command cmd;
    print_flush()
  in
    (*List.fold_left g ctx' cmds*)
    List.iter g cmds

let main () =
  Printexc.record_backtrace true; 
  let inFile = parseArgs() in
  process_file inFile

let () = set_max_boxes 1000
let () = set_margin 67
let res = 
  Printexc.catch (fun () -> 
    try main();0 
    with
       Exit x ->
       onDebug(fun _ -> Printexc.print_backtrace stderr);
       x
     | x ->
      (* onDebug(fun _ -> 
          Printexc.print_backtrace stderr;
          pr "%s" ("Caught exception: "  ^ (Printexc.to_string x) ^ "\n")
       );*)
          Printexc.print_backtrace stderr;
          pr "%s" ("Caught exception: "  ^ (Printexc.to_string x) ^ "\n");

       (-1)
   ) 
  ()
let () = print_flush()
let () = exit res
