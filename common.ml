open Format
open Syntax
open Support.Error
open Support.Pervasive
open Set
open Map
open Printexc

exception NotImplemented
exception Impossible
exception InvalidInput
exception DuplicateMember

(*type get_bound_t = Syntax.ty -> Syntax.ty*)
type get_class_t = Syntax.name -> Syntax.cls
type env_t = (*get_bound_t **) get_class_t 


let substtm = Substitution.substtm
let substtmty = Substitution.substtmty
let substty = Substitution.substty 
let substmethtm = Substitution.substmethtm
let substmethty = Substitution.substmethty
let substfldtm = Substitution.substfldtm
let substfldty = Substitution.substfldty
let substmembertm = Substitution.substmembertm
let substmemberty = Substitution.substmemberty
let substclstm = Substitution.substclstm
let substclsty = Substitution.substclsty
let substmult = Substitution.substmult

(*
module Mctx = Context.Make (
struct 
   type name = myname
   type cls_b = cls

end
)

type context = Mctx.context
*)

type context = unit (* dummy *)

let emptyContext = ()

let getContext decls = raise NotImplemented  

(*
(* get the upper bound of a type variable or the type it self *)
let bound ctx ty = (* get concrete type of ty*) 
   begin match ty with
       TyVar name -> 
         (* getTypeFromContext dummyinfo ctx i *)
         raise NotImplemented 
     | _ -> 
         ty
   end 
*)

(* ----------------------- member map-------------------------- --------------------------------  *)
(*type kind_t = 
     KindMethod of Syntax.ty list (* type signature of a method. Return type comes
     first.*)
   | KindField 

module Member = struct 
  type t = Syntax.name * kind_t
  let compare (n1, k1) (n2, k2) = 
     if (match (k1,k2) with
          (KindMethod tys, KindMethod tys') -> 
             (List.length tys = List.length tys') &&
             (List.for_all Syntax.eqty (List.combine tys tys'))
        ) && (Syntax.eqname n1 n2) then 0 else 1
end

module MemberMap = Map.Make(Member)
(* each member is mapped to its actual definition(s). The definitions 
 * are ORDERED in a list in ascending order (most recent defs come first) *)
type members_t  = Syntax.member list MemberMap.t

let addMember name vl names = 
   if MemberMap.mem name names then 
      raise DuplicateMember 
   else 
      MemberMap.add name vl names

let existsMember name names = MemberMap.mem name names 

let emptyClass = MemberMap.empty
*)
(* ---------------------------------------------------------------------- *)

let members getClass ty = 
 let rec f n ty =
     begin match ty with 
      | TyVar _ ->
         if n = 0 then
            raise InvalidInput
          else 
            raise Impossible 
      | TyObject -> []
      | TyName tyname -> 
         let (name, tyargs) = (tyname2name tyname, tyname2tyargs tyname) in
         let cls = getClass name in
         (*dbg "Commons.getClass 1.:\n %a" Syntax.c2p cls;*)
         let cls =    (* redefine cls after substitution *)
            (* substitute generic arguments in cls s*)
           (*dbg "Common.getClass 2.:\nargs:\t%a ==>\nformals:\t%a" 
           (Syntax.l2p Syntax.ty2s) tyargs 
           (Syntax.l2p Syntax.n2s) (Syntax.clsTyArgs cls) ;*)

            substmult substclsty tyargs (Syntax.clsTyArgs cls) cls 
         in
         let supty = (Syntax.clsSuperTy cls) in
            (*dbg "Commons.getClass 3.:\n %a" Syntax.c2p cls;*)
            List.append (Syntax.clsMembers cls) (f (n+1) supty)
     end
 in
    f 0 ty


(* get fields of a specific class type *)
let fields getClass ty = 
   Syntax.filtmap Syntax.member2fld (members getClass ty)

(* get fields of a specific class type *)
let methods getClass ty = 
   Syntax.filtmap Syntax.member2meth (members getClass ty)


let isClassGraphTree (getClass:Syntax.name->Syntax.cls) (cls:Syntax.cls list) = 
   let rec checkCyclicPath (visited:Syntax.name list) ((name, _, supty, _):Syntax.cls) :
      (Syntax.name list) option =
         let exists = 
            List.exists (fun name' ->  name' = name) visited
         in
            if exists then Some visited
            else begin 
               match supty with 
                 TyObject -> None
               | TyName(name', _) ->
                     checkCyclicPath (name::visited) 
                     (getClass  name')
               | _ -> 
                     raise Impossible
            end

   in
   try List.find ((<>) None)
       (List.map (checkCyclicPath []) cls)
   with 
   Not_found -> None
