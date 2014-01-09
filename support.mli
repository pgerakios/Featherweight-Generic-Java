(* module Support

   Collects a number of low-level facilities used by the other modules
   in the typechecker/evaluator. 
*)

(* ------------------------------------------------------------------------ *)
(* Some pervasive abbreviations -- opened everywhere by convention *)

module Pervasive : sig
  type color = Cyan | Red | Magenta | Yellow | Green 


  val pr : ('a, unit, string, unit) format4 -> 'a
  val prc : color -> ('a, unit, string, unit) format4 -> 'a
end  

(* ------------------------------------------------------------------------ *)
(* Error printing utilities -- opened everywhere by convention *)

module Error : sig

   (* An element of the type info represents a "file position": a 
     file name, line number, and character position within the line.  
     Used for printing error messages. *)
  type info
  val dummyinfo : info

   val i2s : info -> string
   val i2p : unit -> info -> string
   val info :   ('a, unit, string, unit) format4 -> 'a
   val er :   info -> ('a, unit, string, unit) format4 -> 'b
   val warn : info ->  ('a, unit, string, unit) format4 -> 'b
   val dbg :   ('a, unit, string, unit) format4 -> 'a
   val raiseExn : (string -> exn) -> (string, unit, string, string) format4 -> 'b
   val strOf : ('a, unit, string) format -> 'a

   val foundErrors : unit -> bool
   val setDebug : bool -> unit
   val isDebug  : unit -> bool
   val onDebug  : (unit->unit) -> unit
   val dbgTrace : string -> exn -> unit
   val dbgTry   : string -> (exn -> bool) -> (unit -> 'a) -> 'a 
  (* An exception raised by the low-level error printer; exported
     here so that it can be caught in module Main and converted into
     an exit status for the whole program. *)
  exception Exit of int


  val info2pos : info -> (int * int) (* get row, col from position *)


  (* Create file position info: filename lineno column *)
  val createInfo : string -> int -> int -> info
  val printInfo : info -> unit

  (* A convenient datatype for a "value with file info."  Used in
     the lexer and parser. *)
  type 'a withinfo = {i: info; v: 'a}

  (* Print an error message and fail.  The printing function is called
     in a context where the formatter is processing an hvbox.  Insert
     calls to Format.print_space to print a space or, if necessary,
     break the line at that point. *)
  val errf : (unit->unit) -> 'a
  val errfAt : info -> (unit->unit) -> 'a

  (* Convenient wrappers for the above, for the common case where the
     action to be performed is just to print a given string. *)
  val err : string -> 'a
  val error : info -> string -> 'a

  (* Variants that print a message but do not fail afterwards *)
  val warning : string -> unit
  val warningAt : info -> string -> unit
end


