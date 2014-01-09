
type sexn
exception SubstException of sexn
val string_of_sexn : sexn -> string

(* given an abstract substitution function 'a->name->'b->'b
   (that replaces 'a for name in 'b), a list of things to 
   be substituted ('a list), a list of names ('b list) corresponding
   to the things that will be substituted and some entity 'b,
   perform substitutions on the original 'b in a left-to-right manner
 *)
val substmult : ('a->Syntax.name->'b->'b) -> 'a list -> Syntax.name list -> 'b -> 'b

val substtm : Syntax.term -> Syntax.name -> Syntax.term -> Syntax.term
val substtmty : Syntax.ty -> Syntax.name -> Syntax.term -> Syntax.term

val substty : Syntax.ty -> Syntax.name -> Syntax.ty -> Syntax.ty

val substmethtm : Syntax.term -> Syntax.name -> Syntax.meth -> Syntax.meth
val substmethty : Syntax.ty -> Syntax.name -> Syntax.meth -> Syntax.meth

val substfldtm : Syntax.term -> Syntax.name -> Syntax.fld -> Syntax.fld
val substfldty : Syntax.ty -> Syntax.name -> Syntax.fld -> Syntax.fld

val substmembertm : Syntax.term -> Syntax.name -> Syntax.member -> Syntax.member
val substmemberty : Syntax.ty -> Syntax.name -> Syntax.member -> Syntax.member

val substclstm : Syntax.term -> Syntax.name -> Syntax.cls -> Syntax.cls
val substclsty : Syntax.ty -> Syntax.name -> Syntax.cls -> Syntax.cls 
