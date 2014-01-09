exception ViolProgress of string 
exception ViolPreservation of string 

val setUnsafe: bool -> unit
val runSafety: Syntax.decls -> Syntax.term -> unit
