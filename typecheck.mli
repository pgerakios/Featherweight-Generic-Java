
type tyerr
exception TyError of tyerr
val tye2s : tyerr -> string 
val tye2p : unit -> tyerr -> string 

type context
val emptyContext : context


val subTy : context -> Syntax.ty -> Syntax.ty -> bool
val typeOf :  context -> Syntax.term -> Syntax.ty

(* type-check declarations. A TyError exception is raised 
 * if the declarations are not well-typed. The declartions
 * are packed into an abstract well-typed context and 
 * are returned to the environment.
 * *)
val tcDecls : Syntax.decls -> context
