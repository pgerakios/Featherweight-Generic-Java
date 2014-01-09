(* This module contains functionality common to Eval and Typecheck *)

exception NotImplemented
exception Impossible
exception InvalidInput
exception DuplicateMember


(*type get_bound_t = Syntax.ty -> Syntax.ty*)
type get_class_t = Syntax.name -> Syntax.cls
type env_t = (*get_bound_t **) get_class_t 

(* given an environment for mapping names to classes and type vars to their upper
 * bounds, get an ORDERED list of fields for the entire hierarchy of ty. 
 * Most recent definitions precede parent definitions. *)
val fields : env_t -> Syntax.ty -> Syntax.fld list 

(* given an environment for mapping names to classes and type vars to their upper
 * bounds, get an ORDERED list of methods for the entire hierarchy of ty. 
 * Most recent definitions precede parent definitions. *)
val methods : env_t -> Syntax.ty -> Syntax.meth list

(* same as above *)
val members : env_t -> Syntax.ty -> Syntax.member list

(* This function returns None if there exist no cycles otherwise 
 * it returns the first cycle (i.e. a name list) it encounters. 
 * *)
val isClassGraphTree : get_class_t -> 
                       Syntax.cls list -> (Syntax.name list) option
