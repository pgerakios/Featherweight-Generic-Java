(* module Syntax: syntax trees and associated support functions *)
open Support.Pervasive
open Support.Error

type name (* a uniquely defined constant/variable name in the program *)
type names (* a *set* of names *)

(*type context *)

exception NameExists of (name * names) 


(* Data type definitions *)
type ty =
    TyObject
  | TyName of tyname 
  | TyVar  of tyvar

and tyname = name * (ty list)
and tyvar = name

type consubtype = name * ty

type tycon  =  (* type constraint *)
      ConSubtype of consubtype 


type term =
    TmVar of name (* index in context and context length *)
  | TmNew of tmnew 
  | TmInvoke of tminv 
  | TmField of tmfld 

and tmnew = info * ty * name list * term list (* the two lists must be the same*)
and tminv = info * term * name * (term list)
and tmfld = info * term * name

type fld = name * ty 
type meth = name * (name * ty) list *  ty * term

type member = 
    MemField of fld
  | MemMeth  of meth

type tycons = tycon list
type cls = name * tycons * ty * (member list)

type decl =
   DeclClass of cls 

type decls = decl list
type terms = term list
type prog = decls * terms
(*---------------------------------------------------------------------------------------------*)

(* tyname API *)
val tyname2name : tyname -> name
val tyname2tyargs : tyname -> ty list
val mktyname : name -> ty list -> tyname

(* tyvar API *)
val tyvar2name : tyvar -> name
val mktyvar : name -> tyvar

(* Name API *)
val mkname : (string Support.Error.withinfo) -> name (* create a new name *)
val string_of_name : name -> string
val name2info: name -> info
val addName :  name -> names -> names 
val existsName : name -> names -> bool
val emptyNames :  names
val eqname : name -> name -> bool
val cmpname : name -> name -> int 
val getNameScope : name -> int 
val setNameScope : name -> int -> name 
val thisName : name
val ns2s : names -> string (* names to string *)
val ns2p : unit -> names -> string

(* Printing API *)
val prlist: ('a -> unit) -> 'a list -> unit
val printtm: term -> unit
val printty : ty -> unit

(* tycons API *)
val tycons2names : tycons -> names


(* ty API *)
val eqty : ty -> ty -> bool

(* member API *)
val eqmember : member -> member -> bool 
val isField : member -> bool 
val isMethod : member -> bool
val memberNameType : member -> name * (ty list) (* return type first for * methods*)
val member2meth : member -> meth option
val member2fld  : member -> fld option 

(* meth API *)
val methBody : meth -> term
val methArgs : meth -> name list
val methArgTys : meth -> ty list
val eqmeth : meth -> meth -> bool 
val methName : meth -> name 
val methTys : meth -> ty list

(* fld API *)
val eqfld  : fld -> fld -> bool 
val fldName : fld -> name 

(* cls API *)
val clsTyArgs : cls -> name list
val clsName : cls -> name
val clsSuperTy : cls -> ty 
val clsMembers : cls -> member list 
val prcls: cls -> unit

(* Util API *)
val filtmap : ('a -> 'b option) -> 'a list -> 'b list
val eqlist : ('a -> 'b -> bool) -> 'a list -> 'b list -> bool
val tmInfo: term -> info


(* unsorted API*) 
val al2s :  string -> ('a->string) -> 'a list -> string
val al2p :  string -> ('a->string) -> unit -> 'a list -> string

val l2s :  ('a->string) -> 'a list -> string
val l2p :  ('a->string) -> unit -> 'a list -> string
val n2s :  name ->  string
val n2p :  unit -> name ->  string
val tm2p : unit -> term -> string 
val tm2s : term -> string 
val ty2p : unit -> ty -> string
val ty2s : ty -> string
val m2p  : unit -> member -> string
val m2s  : member -> string
val tc2p : unit -> tycon -> string 
val tc2s : tycon -> string
val c2p  : unit -> cls -> string 
val c2s  : cls -> string 
(*val f2p  : unit -> fld -> string
val f2s  : fld -> string*)






(* compare type signatures only and names *)

(*
type binding =
    NameBind
  | ClassBind of cls   (* class definition for some class "Foo" *) 
  | TyVarBind of ty    (* upper bound of type variable "X" *)
  | VarBind of ty        (* type of some variable "x"    *)
*)


(*
(* Contexts *)
val emptycontext : context 
val ctxlength : context -> int
val addbinding : context -> string -> binding -> context
val addname: context -> string -> context
val getbinding : info -> context -> int -> binding
val name2index : info -> context -> string -> int
val isnamebound : context -> string -> bool
val getTypeFromContext : info -> context -> int -> ty


(*val iterbindings : context -> ((string * binding) -> unit) -> unit*)
(*val fmapbindings: (string * binding -> (string * binding) option) -> context
 * -> context*)
val tvar2ctx: context -> string list -> ((ty * ty) list) -> context
val var2ctx: context -> (string * ty) list -> context
val getClass : context -> string -> cls
val getClasses: context -> cls list
*)

(* configurations *)


(* Shifting and substitution *)
(*val termShift: int -> term -> term
val termSubstTop: term -> term -> term
val typeShift : int -> ty -> ty
val typeSubstTop: ty -> ty -> ty
val tytermSubstTop: ty -> term -> term
*)

(*
val prbinding : binding -> unit
val prctx: string -> context -> unit 
*)
