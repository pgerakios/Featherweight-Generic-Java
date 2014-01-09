open Support.Pervasive
open Support.Error

(*
module type Context = sig 
exception ContextError of string

   type name
   type cls_b
   type var_b
   type tvar_b
   type meth_b
   type fld_b

type ('a, 'b, 'c, 'd, 'e) poly_binding =  
    ClassBind of 'a   (* class definition for some class "Foo" *) 
  | TyVarBind of 'b    (* upper bound of type variable "X" *)
  | VarBind   of 'c    (* type of some variable "x"    *)
  | MethBind  of 'd
  | FldBind   of 'e

type level = 
   NewLevel of name        (* new scope *)
 | TopLevel of name        (* topmost scope  index = 0*)
 | AnyLevel of name        (* expresses any scope including NewLevel *) 
 | BottomLevel of name     (* global declarations *)

type binding =  (cls_b, tvar_b, var_b, meth_b, fld_b) poly_binding
type bkey = level
type binding_key = (bkey, bkey, bkey, bkey, bkey) poly_binding
type 'a bpair = level * 'a (* this represents a binding pair to be
inserted to a context: the level indicate where in the context the pair 
should be placed, the name is the key and 'a is the value to be inserted.

NOTICE: if the type of the topmost scope does not match the type of the 
binding, then a new scope will be created
*)
type binding_pair =  (cls_b bpair, 
                      tvar_b bpair, 
                      var_b bpair, 
                      meth_b bpair, 
                      fld_b bpair) poly_binding

type context

val emptycontext : context

(* Add a new lexical scope to the current scope *)
val addScope : context -> context

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
val addBinding : context -> binding_pair -> context

(* get a topmost class from context*)
val getClass : context -> level -> cls_b option

(* get a list of topmost classes*)
val getClasses: context -> (level -> bool) -> cls_b list

end 
*)

module Make = functor (T: sig 
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

end ) -> struct


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

type level = 
   NewLevel         (* new scope *)
 | TopLevel         (* topmost scope  index = 0*)
 | AnyLevel         (* expresses any scope including NewLevel *) 
 | BottomLevel      (* global declarations *)


type binding =  (cls_b, tvar_b, var_b, meth_b, fld_b) poly_binding
type binding_k =  (unit, unit, unit, unit, unit) poly_binding
type binding_v = binding 

type binding_key = level * name * binding_k option
type binding_kval = level * name * binding_v

module Name = struct 
  type t = name
  let compare a b = if T.eq_name a b then 0 else 1
end


module NameMap = Map.Make (Name)
module NameSet = Set.Make (Name)


type vctx = var_b NameMap.t  (* Gamma context*)
type tctx = tvar_b NameMap.t  (* Delta context*)
type dctx = cls_b NameMap.t (* class context*)
type mctx = meth_b NameMap.t
type fctx = fld_b NameMap.t 

type scope =  (dctx, tctx, vctx, mctx, fctx) poly_binding
   
let prctx cname ctx  = 
  let prscope (scope:scope) =
         let prm map fn = 
            NameMap.iter (fun name vl ->
                        pr "Name: %a |-> %a\n" 
                        (fun () -> T.string_of_name) name fn vl
                       ) map
         in
            pr "BEGIN SCOPE\n";
            begin match scope with 
               ClassBind dctx -> prm dctx (fun () -> T.prcls_b)
             | TyVarBind tctx -> prm tctx (fun () -> T.prtvar_b)
             | VarBind   vctx -> prm vctx (fun () -> T.prvar_b)
             | MethBind  mctx -> prm mctx (fun () -> T.prmeth_b)
             | FldBind   fctx -> prm fctx (fun () -> T.prfld_b)
            end;
            pr "END SCOPE.\n" 
 
  in
     pr "BEGIN Print context \"%s\":\n" cname;
     List.iter prscope ctx;
     pr "END Print context \"%s\"\n" cname 

let cexn = 
   fun ctx s -> onDebug (fun () ->  (prctx "???" ctx)); raise (T.exnFn s)


(* NOTE: This representation of scopes could be flattened by 
 * performing alpha-renaming.*)
type context = scope list  (* stack-based scopes *)


let emptyscope = NameMap.empty

let emptycontext = []

let addContexts ctx1 ctx2 = List.append ctx1 ctx2

let fnd name (map: 'a NameMap.t) = 
  try Some(NameMap.find name map)
  with Not_found -> None 

let add (map: 'a NameMap.t) name v = 
   begin match fnd name map with 
      None ->
         NameMap.add name v map
    | Some _ ->
         cexn emptycontext ((T.string_of_name name) ^ " already exists in context.")
   end 

let getBinding (ctx:context) ((level, name, binding):binding_key) =
  if ctx = [] then 
     None (* trival case nothing to be searched *)
  else 
  let ctxLen = List.length ctx in
  let levelOK levelID = 
       (level = TopLevel && ctxLen > 0 && levelID = 0) ||
       (level = AnyLevel && ctxLen > 0 && levelID >= 0 && levelID < ctxLen) ||
       (level = BottomLevel && ctxLen > 0 && levelID = ctxLen - 1)
  in   
  let findScope levelID (scope:scope) =
       let guardOK g = 
          (levelOK levelID) &&
          (binding=None || binding = Some g) 
       in
       let n2n fn map = 
          begin match fnd name map with
              None -> None
            | Some x -> Some (fn x) 
          end 
       in
       (*onDebug(fun () -> 
       pr "Context : guardOK: Class=%b TyVar=%b Var=%b Meth=%b Fld=%b" 
       (guardOK (ClassBind ())) 
       (guardOK (TyVarBind ())) 
       (guardOK (VarBind ())) 
       (guardOK (MethBind ())) 
        (guardOK (FldBind ())) ) ;*)
       begin match scope with 
            ClassBind dctx when guardOK (ClassBind ()) ->
               n2n (fun x -> ClassBind x) dctx
         |  TyVarBind tctx when guardOK (TyVarBind ()) ->
               n2n (fun x -> TyVarBind x) tctx
         |  VarBind vctx when guardOK (VarBind ()) ->
               n2n (fun x -> VarBind x) vctx
         |  MethBind mctx when guardOK (MethBind ()) ->
               n2n (fun x -> MethBind x) mctx
         |  FldBind fctx when guardOK (FldBind ()) ->
               n2n (fun x -> FldBind x) fctx
         |  _ ->
            None
      end
  in
    snd (List.fold_left
    (fun (levelID, binding) scope ->
      (*prc Magenta "binding <> None = %b\n" (binding <> None);*)
      if binding = None then
        (levelID+1, findScope levelID scope)
      else 
        (levelID, binding)
    ) (0, None) ctx)


    (* TOD: why not make binding pair ->
       * (level * binding)  ???? it is much simpler 
       * *)
let addBinding (ctx:context) (level, name, (binding:binding)) =
   let tryAddScope scope = 
      begin match (scope, binding) with
            (ClassBind dctx, ClassBind cls) ->
               Some (ClassBind (add dctx name cls))
         |  (TyVarBind tctx, TyVarBind tv) ->
               Some (TyVarBind(add tctx name tv))
         |  (VarBind vctx, VarBind v) -> 
               Some (VarBind (add vctx name v))
         |  (MethBind mctx, MethBind m) ->
               Some (MethBind (add mctx name m))
         |  (FldBind fctx, FldBind f) ->
               Some (FldBind (add fctx name f))
         |  _ -> 
               None 
      end
   in
   let newScope () = 
      let add v =  add emptyscope name v in
      begin match binding with 
         ClassBind cls -> 
            ClassBind (add cls)
       | TyVarBind tv ->
            TyVarBind (add tv)
       | VarBind v ->
            VarBind (add v)
       | MethBind m ->
            MethBind (add m)
       | FldBind f ->
            FldBind (add f) 
      end
   in
   let addTopScope ctx = 
       begin match ctx with
          [] -> (newScope ())::[]
        | scope::ctx ->  
           begin match tryAddScope scope with
              Some scope -> scope :: ctx
            | None -> (newScope ())::scope::ctx
           end
       end
   in
   begin match level with 
       NewLevel -> List.append (addTopScope []) ctx
     | AnyLevel (* any level is equivalent to top level*)
     | TopLevel -> addTopScope ctx
     | BottomLevel -> List.rev (addTopScope (List.rev ctx))
   end

(*
(* FIXME: Dummy implementation just to keep the linker happy. *)
let tvar2ctx ctx tvars constraints = 
   ctx
  
(* FIXME: Dummy implementation just to keep the linker happy. *)
let var2ctx ctx argtys = 
   ctx
*)

let getClass (ctx:context) (level, name) =
   begin match getBinding ctx (level, name, Some (ClassBind ())) with 
      Some(ClassBind cls) -> cls
    | Some _ ->  
      cexn ctx ("Class " ^ (T.string_of_name name) ^ " does not exist in context
      (but it does exist as a name binding).")
    | None ->
      cexn ctx ("Class " ^ (T.string_of_name name) ^ " does not exist in context.")
   end 

let getClasses (ctx:context) (fn:(level*name)->bool) =
   let ln = List.length ctx in 
   let n2l n = 
      if n = ln - 1 then BottomLevel
      else if n = 0 then TopLevel
      else if n = ln - 1 then BottomLevel
      else  AnyLevel
   in
   snd (List.fold_left (fun (n, ret) scope ->
     begin match scope with 
       ClassBind(dctx) ->
         (n+1, NameMap.fold (fun name cls ret -> 
                  if fn (n2l n, name) then cls::ret
                  else ret
               ) dctx ret)
     | _ -> (n, ret)
    end
   ) (0, []) ctx)
end 
