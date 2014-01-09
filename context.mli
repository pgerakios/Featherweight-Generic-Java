open Support.Error

(*
 *  A complete implementation for nested and extensible contexts
 *
 *
 * *)


module Make:
   functor (T: 
sig 
   type name
   type cls_b
   type var_b
   type tvar_b
   type meth_b
   type fld_b

   val exnFn : string -> exn
   val eq_name : name -> name -> bool
   val string_of_name :  name -> string

   val prcls_b  : cls_b -> string
   val prvar_b  : var_b -> string
   val prtvar_b : tvar_b -> string
   val prmeth_b : meth_b -> string
   val prfld_b  : fld_b -> string
end ) -> sig


 type name  = T.name
 type cls_b = T.cls_b
 type var_b = T.var_b
 type tvar_b = T.tvar_b
 type meth_b = T.meth_b
 type fld_b = T.fld_b

type ('a, 'b, 'c, 'd, 'e) poly_binding =  
    ClassBind of 'a   (* class definition for some class "Foo" *) 
  | TyVarBind of 'b    (* upper bound of type variable "X" *)
  | VarBind   of 'c    (* type of some variable "x"    *)
  | MethBind  of 'd
  | FldBind   of 'e

type level  = 
   NewLevel         (* new scope *)
 | TopLevel         (* topmost scope  index = 0*)
 | AnyLevel         (* expresses any scope including NewLevel *) 
 | BottomLevel      (* global declarations *)


type binding =  (cls_b, tvar_b, var_b, meth_b, fld_b) poly_binding
type binding_k =  (unit, unit, unit, unit, unit) poly_binding
type binding_v = binding 

type binding_key = level * name *  binding_k option
type binding_kval = level * name * binding_v

(*(bkey, bkey, bkey, bkey, bkey)
poly_binding*)
(* this represents a binding pair to be
inserted to a context: the level indicate where in the context the pair 
should be placed, the name is the key and 'a is the value to be inserted.

NOTICE: if the type of the topmost scope does not match the type of the 
binding, then a new scope will be created
*)

type context

val emptycontext : context

(* the lexical scope(s) of the first argument are 
 * placed before the scope(s) of the second argument.
 * *)
val addContexts : context -> context -> context 


(* return the binding if the name is identical and whether a boolean
 * indicating whether the name occurs in the innermost scope. The 
 * boolean aids in implementing variable shadowing. 
 * *)
val getBinding : context -> binding_key -> binding option

(* the first argument says whether a new context sho*)
val addBinding : context -> binding_kval -> context

(* get a topmost class from context*)
val getClass : context -> (level * name) -> cls_b

(* get a list of topmost classes*)
val getClasses: context -> ((level * name) -> bool) -> cls_b list

val prctx : string -> context -> unit  

end 
