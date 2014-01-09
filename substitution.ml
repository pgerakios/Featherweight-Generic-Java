open Syntax
open Support.Error
(* Assumption: 
   *
   *  Types do not contain terms. Substitution can be modified to support this
   *  feature.
   * *)
exception NotImplemented 
exception Impossible
type sexn = string
exception SubstException of sexn


(*------------------------- Exceptions  ---------------------------------------------------*)
let sexn s =
(*   raiseExn (fun s -> SubstException s) format*)
   raise (SubstException s) 

let  string_of_sexn (s:sexn) = s

let substmult (fn: 'a->name->'b->'b) (a:'a list) (n: name list) (b:'b) =
   if (List.length a) = (List.length n) then
   begin
      List.fold_left (fun b (a, n) -> fn a n b ) b (List.combine a n)
   end
   else
      sexn (strOf "substmult: invalid lengths  %d and %d" 
                  (List.length a) (List.length n))

(*------------------------- Shifting  ---------------------------------------------------*)

(* transform a list of items with an intial environment and yield an output
 * environment
 * *)
let envList (fn: 'env -> 'a -> ('env * 'a)) (env:'env) (a: 'a list) =
 let fld (env, a') a = 
    let env, a = fn env a 
    in 
       env, a::a'
 in
 let (env, a) = List.fold_left fld (env, []) a 
 in
    (env, List.rev a) 

type 'env vfun = (* visitor function *)
     VisTm of ('env -> name -> ('env * term option))
   | VisTy of ('env -> name -> ('env * ty option))

let vapp_tm (vfun: 'env vfun) (env:'env) (name:name) =
   begin match vfun with 
       VisTm fn -> fn env name 
     | _ -> env, None 
   end

let vapp_ty (vfun: 'env vfun) (env:'env) (name:name) =
   begin match vfun with 
       VisTy fn -> fn env name 
     | _ -> env, None 
   end

let rec tmVarVisitor (fn: 'env vfun) (env:'env) (tm:term) =
 let rec f env tm =
   begin match tm with
     TmVar n ->
           begin match vapp_tm fn env n with
              (env, Some tm) -> env, tm
            | (env, None) -> env, tm
           end
   | TmNew(i, ty, names, terms) ->
        let ty0 = ty in
        let env, ty = tyVarVisitor fn env ty in
        let env, terms = envList f env terms in
        (env, TmNew(i, ty, names, terms))
   | TmInvoke (i, tm, name, tms) ->
        let env, tm = f env tm in
        let env, tms = envList f env tms 
        in
           env, TmInvoke (i, tm, name, tms)
   | TmField(i, tm, name) -> 
         let env, tm = f env tm in
         env, TmField(i, tm, name) 
   end
 in 
    f env tm

and tyVarVisitor (fn: 'env vfun) (env:'env) (ty:ty) = 
  let rec f env ty =
     begin match ty with 
         TyObject -> env, TyObject
       | TyName (name, tys) ->
             let env, tys = envList f env tys in
             env, TyName (name, tys)
       | TyVar name -> 
         begin match vapp_ty fn env name with 
            env, Some ty -> env, ty
          | env, None -> env, ty
         end
     end
  in
     f env ty

let n2n = string_of_name
let n2s (nameOpt : name option) n =
      begin match nameOpt with
         None -> getNameScope n
       | Some n' when  (n2n n') = (n2n n) -> getNameScope n
       |  _ -> 0
      end
let max n1 n2 = if n1 > n2 then n1 else n2 

let tmMaxScope (nameOpt:name option) (tm:term) =
 let fn max_n name = 
    max max_n (n2s nameOpt name), None
 in
    fst(tmVarVisitor (VisTm fn) 0 tm)

let tyMaxScope nameOpt (ty:ty) =
 let fn max_n name = 
    max max_n (n2s nameOpt name), None
 in
    fst(tyVarVisitor (VisTy fn) 0 ty)


let s2n (nameOpt : name option) (s2:int) (n:name) = (* assuming n2 >= scope of n*)
     let st () =
         setNameScope n (s2 + (getNameScope n))
     in
      begin match nameOpt with
         None -> st ()
       | Some n' when  (n2n n') = (n2n n) -> st ()
       |  _ -> n
      end

let tmSetScope (nameOpt:name option) (scope:int) (tm:term) =
 let fn () name = (), Some (TmVar (s2n nameOpt scope name))
 in
    snd(tmVarVisitor (VisTm fn) () tm)

let tySetScope nameOpt scope (ty:ty) =
 let fn () name = 
    (), Some (TyVar (s2n nameOpt scope name))
 in
    snd(tyVarVisitor (VisTy fn) () ty)
(*---------------------------------------------------------------------------------------*)

(* abstract substitution algorithm that operates on any data structure given the
 * first three arguments 
 *
 * Assumption: we are assuming here that all free variables have the 
 * same scope. The order of substitution of same-scope variables 
 * is therefore irrelevant   * as they are all incremented by 
 * the same amount simultaneously.
 *
 * *)
let absSubst (fnMaxScope_b : name option -> 'b -> int)
             (fnMaxScope_a : name option -> 'a -> int)
             (fnSetScope : name option -> int -> 'a -> 'a )
             (fnVarVisitor : ('env -> name -> ('env * 'b option)) -> 'env -> 'a -> ('env * 'a)) 
             (e2:'b) (name:name) (e1:'a) =

   let e1_max = fnMaxScope_a None e1 in 
   let e2_max = fnMaxScope_b None e2 in
   let e_max = (max e1_max e2_max) + 1 in
   let e1 = fnSetScope None e_max e1 in
   let name = (* make the same scope for name to be subst*) 
      Syntax.setNameScope name  (fnMaxScope_a (Some name) e1)
      (*((Syntax.getNameScope name) + e_max) *)
   in
   let subVar () name' = (), 
      (let eq = eqname name name' 
       in 
         (*dbg "Substitution.subVar: Matches %a (%d) with mine %a (%d) ? %b"
         Syntax.n2p name  (Syntax.getNameScope name)
         Syntax.n2p name' (Syntax.getNameScope name')
         eq;*)

         if eq then Some e2 else None
      )  
   in
      (snd (fnVarVisitor subVar () e1):'a)

(* term substitution in term *) 
let substtm (tm2:term) (name:name) (tm1:term) =
   let tmVarVisitor a b c = tmVarVisitor (VisTm a) b c 
   in
      absSubst tmMaxScope tmMaxScope tmSetScope tmVarVisitor tm2 name tm1 
  
(* type substitution in term *)
let substtmty (ty:ty) (name:name) (tm:term) = 
   let tmVarVisitor a b c = tmVarVisitor (VisTy a) b c 
   in
      absSubst tyMaxScope tmMaxScope tmSetScope tmVarVisitor ty name tm 


(* substitute type in type *)
let substty ty name ty' = 
  let tyVarVisitor a b c = tyVarVisitor (VisTy a) b c 
   in
      absSubst tyMaxScope tyMaxScope tySetScope tyVarVisitor ty name ty'

(* substitute term for some formal method parameter *)
let substmethtm (tm1:term) (n:name) 
                ((name, argTys, retTy, tmBody):meth) =
   let n = (* find the proper name in the method list *)
      if Syntax.eqname Syntax.thisName n then n
      else 
      try
         fst (List.find 
             (fun (n', _) -> (string_of_name n') =
                           (string_of_name n)) argTys)
      with Not_found -> 
         begin 
            sexn (strOf
                  "substmeththm: name %s not found in args of meth %s" 
                        (string_of_name n) (string_of_name name))
         end
   in 
      (name, argTys, retTy, substtm tm1 n tmBody)      


(* substitute type in method body, method parameter types and return type*)
let substmethty (ty:ty) (n:name)
                ((name, argTys, retTy, tmBody):meth) = 
 
      (name, List.map (fun (n', ty') -> n', substty ty n ty') argTys, 
       substty ty n retTy, substtmty ty n tmBody)      

(* there are no free term variables in a field*)
let substfldtm (tm1:term) (n:name)(f:fld) = f

(* substitute type variable of a field with another type.*)
let substfldty (ty:ty) (n:name) ((fn, fty):fld) = 
   (fn, substty ty n fty)

(* substitute term in a class member *)
let substmembertm (tm1:term) (n:name)(m:member) = 
  begin match m with 
      MemField f ->  MemField (substfldtm tm1 n f)
    | MemMeth  m ->  MemMeth  (substmethtm tm1 n m)
  end  

(* substitute type in a class member *)
let substmemberty (ty:ty) (n:name) (m:member) = 
  begin match m with 
      MemField f ->  MemField (substfldty ty n f)
    | MemMeth  m ->  MemMeth  (substmethty ty n m)
  end  

(* substitute a free term variable in a class definition. *)
let substclstm (tm1:term) (n:name) 
               ((name, tycons, supty, mems):cls) = 
 (name, tycons, supty, List.map (substmembertm tm1 n) mems)                  
                  
(* substitute a type variable in a class definition *)
let substclsty (ty1:ty) (n:name) 
               ((name, tycons, supty, mems):cls) = 
 (name, tycons, substty ty1 n supty, 
  List.map (substmemberty ty1 n) mems)                  
