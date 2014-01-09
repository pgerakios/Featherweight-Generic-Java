(* module Core

   Core typechecking and evaluation functions
*)

open Syntax

val eval : decls -> term -> term 
val subtype : ty -> ty -> bool
val isval:  term -> bool
