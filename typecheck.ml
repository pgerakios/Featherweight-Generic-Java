open Format
open Support.Error
open Support.Pervasive
open Set
open Printexc
open Context
open Substitution

exception NotImplemented
exception Impossible

type tyerr = (* this type is abstracted from the interface.
                Thus, only here can we throw a tyerr *) 
   TyeCycle of Syntax.name list
 | TyeDupField of Syntax.name
 | TyeInvOverride of Syntax.name
 | TyeRetTySubtype of Syntax.ty * Syntax.name * Syntax.ty
 | TyeMethodNotFound of Syntax.name * Syntax.ty
 | TyeTyVarNotFound of Syntax.name
 | TyeVarNotFound of Syntax.name
 | TyeNewMismatch of Syntax.name list * Syntax.term list *
                     Syntax.name list * Syntax.ty list
 | TyeInvokeSubtypes of Syntax.ty list * Syntax.ty list
 | TyeFieldNameNotFound of Syntax.name * Syntax.ty 
 | TyeContext of string

exception TyError of tyerr

let rstye tyerr = 
   raise (TyError tyerr)

(* printing function for error messages *)
let tye2s = function
   TyeCycle names -> strOf "Circular class definition: %a" (Syntax.l2p Syntax.n2s) names 
 | TyeDupField n -> strOf "Duplicate field name in class hierarchy. Field name %a" Syntax.n2p n
 | TyeInvOverride n -> strOf "Invalid method override. Method name %a" Syntax.n2p n
 | TyeRetTySubtype (ty1, methName, ty2) -> 
       strOf "[%a] Return type %a differs from actual return type %a in function %a"
    i2p (Syntax.name2info methName)
    Syntax.ty2p ty2
    Syntax.ty2p ty1 Syntax.n2p methName
 | TyeMethodNotFound(n, ty) -> 
    strOf "Method %a could not be found in class %a."
    Syntax.n2p n Syntax.ty2p ty
 | TyeTyVarNotFound name ->
    strOf "Type variable %a could not be found in context."
    Syntax.n2p name
 | TyeVarNotFound name ->
    strOf "Variable %a could not be found in context."
    Syntax.n2p name
 | TyeNewMismatch (argNames, args, fieldNames, fieldTys) ->
       strOf "Term new error: Could not match [%a] with [%a]" 
   (Syntax.l2p (fun (n, tm) -> strOf "%a=%a" Syntax.n2p n Syntax.tm2p tm))
   (List.combine argNames args) 
   (Syntax.l2p (fun (n, ty) -> strOf "%a:%a" Syntax.n2p n Syntax.ty2p ty))
   (List.combine fieldNames fieldTys) 

 | TyeInvokeSubtypes(formalTys, actualTys) ->
   strOf "TyeInvokeSubtypes FIXME" (* FIXME !!!! *)
 | TyeFieldNameNotFound(fldName, clsTy) -> 
   strOf "TyeFieldNameNotFound FIXME" (* FIXME !!!! *)
 | TyeContext cerr ->
   strOf "%s"  cerr

let tye2p = fun () -> tye2s

module TyCtx = Context.Make (
struct 
   type name   = Syntax.name
   type cls_b  = Syntax.cls 
   type var_b  = Syntax.ty 
   type tvar_b = Syntax.tycon 
   type meth_b = Syntax.meth 
   type fld_b  = Syntax.fld

   let exnFn = (fun s -> TyError(TyeContext s))
   let eq_name = Syntax.eqname 
   let string_of_name = Syntax.n2s
   let prcls_b = Syntax.c2s
   let prvar_b = Syntax.ty2s
   let prtvar_b = Syntax.tc2s
   let prmeth_b m = Syntax.m2s (Syntax.MemMeth m) 
   let prfld_b  f = Syntax.m2s (Syntax.MemField f)
end
)

type context = TyCtx.context

open Syntax

let emptyContext = TyCtx.emptycontext

(*------------------ Helper functions ----------------------------*)
let decls2ctx decls = 
  let addb ctx (Syntax.DeclClass cls) =  
     TyCtx.addBinding ctx 
     (TyCtx.TopLevel, Syntax.clsName cls, TyCtx.ClassBind cls)
  in
     List.fold_left addb emptyContext decls

let argTys2ctx (ty:ty) 
               (l: (Syntax.name * Syntax.ty) list) : context =
  let addb ctx (name, ty) =  
     TyCtx.addBinding ctx 
     (TyCtx.TopLevel, name, TyCtx.VarBind ty)
  in
     List.fold_left addb emptyContext ((Syntax.thisName, ty)::l)


let getClass ctx name = 
      TyCtx.getClass ctx (TyCtx.AnyLevel, name)

let getTyVarBound ctx name = 
  begin match   TyCtx.getBinding ctx 
      (TyCtx.TopLevel, name, Some (TyCtx.TyVarBind ()))
  with 
      Some (TyCtx.TyVarBind(Syntax.ConSubtype (_, ty))) -> ty 
    | _ -> rstye (TyeTyVarNotFound name)
  end

let getVarTy ctx name : Syntax.ty = 
  begin match   TyCtx.getBinding ctx 
      (TyCtx.TopLevel, name, Some (TyCtx.VarBind ()))
  with 
      Some (TyCtx.VarBind(ty)) -> ty 
    | _ -> rstye (TyeVarNotFound name)
  end

(* As defined in the FGJ paper *)
let bound ctx ty = (* get concrete type of ty*) 
   begin match ty with
       TyVar name -> getTyVarBound ctx name
     | _ -> ty
   end 

(*----------------------------------------------------------------*)

(* mtype function as defined in the original paper. 
 * We avoid recursion by obtaining a flattend list
 * of all methods in the hierarchy *)
let mtype ctx name ty = 
 let methods = Common.methods (getClass ctx) ty in
 let meth = 
   try
    List.find (fun m -> Syntax.eqname name (Syntax.methName m))
    methods
   with Not_found -> 
      rstye (TyeMethodNotFound(name, ty))
 in
 let methTys = Syntax.methTys meth 
 in
   (List.tl methTys, List.hd methTys)


(*----------------------------------------------------------------*)
(* well-formedness definition *)
let rec wfTy (ctx:context) (ty:ty) : unit = 
 begin match ty with 
    TyObject -> ()
  | TyName (n, tys) ->
        List.iter (wfTy ctx) tys
  | TyVar  n ->
        let _ =  getTyVarBound ctx n in () 
 end

(*----------------------------------------------------------------*)
and tcTerm (ctx:context) (tm:term) : ty =  
 begin match tm with 
    TmVar name -> getVarTy ctx name 
  | TmNew(i, ty, names, args) -> 
 
    wfTy ctx ty; (*Step 1: type of new must be well-formed *)

    (* Step 2:create the list of actuals and the list of formals, 
     * sort them by name *)
    let cmptup : 'a 'b. (Syntax.name * 'a) ->
                        (Syntax.name * 'b) -> int = 
      (fun (n1, _) (n2, _) -> Syntax.cmpname n1 n2)
    in
    (*prc Magenta "\nTmNew %a\n" Syntax.tm2p tm;*)
    let namedArgs =  List.sort cmptup  (List.combine names args) in
    let fields = List.sort cmptup (Common.fields (getClass ctx) ty) in
    (*Step 3; compare each element of the two lists *)
    let eqfn ((argName:name),(arg:term)) 
             ((fldName, fldTy):fld) : bool = 
       let c1 = Syntax.eqname argName fldName in
       let argTy =  tcTerm ctx arg in
       let c2 = subTy ctx argTy fldTy in
       c1 && c2
    in  
    (* Step 4: Compare the two lists for equality  *)
    if Syntax.eqlist eqfn namedArgs fields then ty 
    else begin
         let argNames, args = List.split namedArgs in
         let fieldNames, fieldTys = List.split fields in
         rstye (TyeNewMismatch(argNames, args, fieldNames, fieldTys))
    end
  | TmInvoke(fi, tm, memberName, args) -> 
      let tmTy = bound ctx (tcTerm ctx tm) in (* type check term*)
      let (formalTys, retTy) = mtype ctx memberName tmTy in
      let argTys = List.map (tcTerm ctx) args in 
       (* actual argument types must be subtypes of formal arg types*)
      if Syntax.eqlist (subTy ctx) argTys formalTys then
         retTy
      else
         rstye (TyeInvokeSubtypes(formalTys, argTys))
  | TmField(fi, tm, memberName) ->
      let tmTy = bound ctx (tcTerm ctx tm) in (* type check term*)
      let (_, fldTy) = 
        try 
          List.find (fun (fldName, _) -> 
             Syntax.eqname fldName memberName)
            (Common.fields (getClass ctx) tmTy)
        with Not_found -> 
            rstye (TyeFieldNameNotFound(memberName, tmTy))
       in
         fldTy
 end

(*----------------------------------------------------------------*)

and eqTy ctx ty1 ty2 =  (subTy ctx ty1 ty2) && (subTy ctx ty2 ty1)

(*----------------------------------------------------------------*)
and subTy ctx ty1 ty2 = (ty2 = TyObject) ||
   (ty1 != TyObject &&
   (let ty1 = bound ctx ty1 in
    let ty2 = bound ctx ty2 in
      match ty1, ty2 with 
        (TyName (n1, l1), TyName (n2, l2)) ->

           if Syntax.eqname n1 n2 then 
             Syntax.eqlist (eqTy ctx) l1 l2
           else
              let cls = getClass ctx n1 in
              let tyArgs = Syntax.clsTyArgs cls in
              let supTy = Syntax.clsSuperTy cls in 
              let supTy' = Substitution.substmult
                  Substitution.substty l1 tyArgs supTy
              in
                 subTy ctx supTy' ty2
      | _ -> raise Impossible 
           
   ))

(*----------------------------------------------------------------*)
(* type checking rules for class member declarations *)
let tcMember ctx clsName member : unit = 
   begin match member with 
      MemField(_, ty) ->  wfTy ctx ty (* field type is well formed*)
    | MemMeth(methName, typedArgs, retTy, body) ->
      (* Step 1: get class type variables and class type *)
      let clsTys = Syntax.clsTyArgs (getClass ctx clsName) in
      let clsTy = Syntax.TyName(clsName, 
                  List.map (fun x -> Syntax.TyVar x) clsTys) 
      in

      (*Step 2: get formal arg types, retty and class type 
       * and check they are well-formed*)
      List.iter (wfTy ctx)  (clsTy::retTy::(List.map snd typedArgs));

      (*Step 3: extend context with this/arg types *) 
      let ctx' = argTys2ctx clsTy typedArgs in 
      let ctx'' = TyCtx.addContexts ctx' ctx in 
      (* Step 4: type-check method body in extended context
       * and get return type*) 
      let retTy' = tcTerm ctx'' body in (* type-check method body*)

      (* Step 5: check the actual return type is a subtype of 
       * the declared return type*)
      if not (subTy ctx retTy' retTy) then 
        rstye (TyeRetTySubtype(retTy, methName, retTy'))
   end

(*----------------------------------------------------------------*)
(* validOverride predicate almost identical to the original
 * rule described in Igarashi et. al FGJ paper. *)
let validOverride (getClass : Syntax.name -> Syntax.cls) 
                  (name : Syntax.name) : unit = 
 let members = Common.members getClass (TyName (name, [])) in
 let methods = Syntax.filtmap Syntax.member2meth members in
 let fields =  Syntax.filtmap Syntax.member2fld members 
 in (* Now we have the flat hierarchy of methods and fields
       including members of all parent classes *)
 (* predicate determining that all fields are uniquely defined*) 
 let uniqField fields (name, _) = 
    if List.exists (Syntax.eqname name) fields then
       rstye (TyeDupField name)
    else 
       name::fields 
 in
 (* predicate determining that all methods are uniquely defined.*)
 let validMeth methods m = 
    let name, tys = Syntax.memberNameType (MemMeth m) in
    let invalid m = 
       let name', tys' = Syntax.memberNameType (MemMeth m) in
       (Syntax.eqname name name') && (not (eqlist eqty tys tys'))
    in
       if List.exists invalid methods then 
          rstye (TyeInvOverride name)
       else 
          m::methods
 in 
 let _ = List.fold_left uniqField [] fields in(* if exception duplicate fields*)
 let _ = List.fold_left validMeth [] methods 
 in 
    ()


(*----------------------------------------------------------------*)
(* type-check class declaration *)
let tcCls ctx ((name, constraints, superty, members) as cls) =
  dbg "Type checking class: %a\n%a" Syntax.n2p name Syntax.c2p cls;

  wfTy ctx superty; (* super type is well formed  *) 
  List.iter 
  (function Syntax.ConSubtype(name, sup) -> wfTy ctx sup) 
  constraints; (* well-formedness tvars supertypes *)

  (* Add type variable constraints to the typing context *)
  let ctx = List.fold_left 
    (fun ctx -> function Syntax.ConSubtype(name, sup) -> 
      let bnd = TyCtx.TyVarBind (ConSubtype(name, sup)) in
      TyCtx.addBinding ctx (TyCtx.TopLevel, name, bnd)) ctx
      constraints 
  in
  let ctx = (* add another scope to our context consisting of 
               fields and methods. If there is a duplicate
               method or field an error will be thrown.
               NOTICE: this ctx is not necessary and can be 
               commented out. It is convenient for looking
               up methods/fields without using this.member.
            *)
      List.fold_left 
      (fun ctx member ->
         let name = fst (Syntax.memberNameType member) in
         let bnd = 
            begin match member with 
                Syntax.MemField fld -> TyCtx.FldBind fld
              | Syntax.MemMeth meth -> TyCtx.MethBind meth
            end 
         in 
            TyCtx.addBinding ctx (TyCtx.TopLevel, name, bnd)) 
      ctx members;
  in
     (* check the validity of method overrides and uniqueness
      *  of fields *)
     validOverride (getClass ctx) name;
     (* type check is class member *)
     List.iter (tcMember ctx name) members 


(*----------------------------------------------------------------*)
(* type-check declarations *)
let tcDecls decls =
   let ctx = (decls2ctx decls:context) in (* convert decls to context*) 
   let topCls = (* get top-level classes *)
     TyCtx.getClasses ctx 
     (function  (TyCtx.TopLevel, _) -> true | _ -> false)
   in (* check for cycles starting from top-level classes *)
      begin match Common.isClassGraphTree (getClass ctx) topCls with     
         Some names -> rstye (TyeCycle names)
       | None -> () 
      end;
   (* check that each decl is well-typed*)
   List.iter (tcCls ctx) topCls;
   ctx (* return well-typed context to the environment *)
(*----------------------------------------------------------------*)


let typeOf = tcTerm
