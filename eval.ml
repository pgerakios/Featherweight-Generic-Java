open Format
open Syntax
open Support.Error
open Support.Pervasive
open Set
open Printexc


exception NotImplemented
exception Impossible
exception NoRuleApplies

type rkind = RedField  | RedMethod
type redex = (ty * name list  * term list) * rkind * name * term list
type 'env rfun = 'env -> redex -> ('env * term)

let rec isval t = match t with
    TmNew(_, _, _, ntl) -> List.for_all (fun t -> isval t) ntl
  | _ -> false

let getval t = 
   begin match t with 
      TmNew (_, ty, names, tms) when isval t -> 
         (ty, names, tms)
     | _ -> raise NoRuleApplies
   end 

let rec tmRedVisitor (fn: 'env rfun) (env:'env) (tm:term) =
 let rec f env tm =
   let l tms = (* evaluate the first non-val in a list of terms*)
      let env, ntl, found = List.fold_left
          (fun (env, ntl, found) tm ->
             if found then env, tm::ntl, true
             else if isval tm then env, tm::ntl, false
             else let env, tm = f env tm in env, tm::ntl, true
          ) (env, [], false) tms
      in
         if not found then raise NoRuleApplies
         else env, List.rev ntl
   in
   let arevals l = List.for_all isval l in (*true is all items are vals*)
   begin match tm with
     TmVar _ -> raise NoRuleApplies
   | TmNew _ when isval tm -> raise NoRuleApplies
   | TmNew(i, ty, names, tms) -> 
         let env, tms = l tms (* reduce arguments*)
         in env, TmNew(i, ty, names, tms)
   | TmField(i, tm, name) when isval tm -> (* reduction rule*)
         fn env (getval tm, RedField, name, [])
   | TmField(i, tm, name) ->
         let env, tm = f env tm in 
         env, TmField(i, tm, name)
   | TmInvoke (i, tm, name, tms) when arevals (tm::tms) -> (* red-rule*)
         fn env (getval tm, RedMethod, name, tms)
   | TmInvoke (i, tm, name, tms) ->
        let env, tms = l (tm::tms) in (* evaluate term list *)
        env, TmInvoke(i, List.hd tms, name, List.tl tms)
   end
 in 
    f env tm



let eval decls term = (* perform a single step *)
   let getClass name = 
      try 
         let cls =  begin match List.find 
             (function DeclClass cls ->                
               Syntax.eqname (Syntax.clsName cls) name) decls 
             with DeclClass cls -> cls  end
         in
            (*dbg "Eval.getClass: %a" Syntax.c2p cls;*)
            cls
      with  Not_found -> 
      begin
         er dummyinfo 
         "Run-time error: could not find class %a" Syntax.n2p name
      end
   in
   let reductionRule () (((ty, names, terms), rkind, name, args):redex) =
      (), 
      begin match rkind with 
         RedField -> (* reduction rule for field access *)
          (* if field does not exist in class then  abort*)
          if not (List.exists 
                 (fun f -> Syntax.eqname (Syntax.fldName f)  name) 
                  (Common.fields getClass ty)) then
                   raise NoRuleApplies
          else begin try
              snd (List.find (fun (n, tm) -> Syntax.eqname n name) 
                   (List.combine names terms))
            with Not_found -> raise NoRuleApplies 
          end

       | RedMethod -> (* reduction rule for method access *)
         let meth = 
            List.find (fun m -> Syntax.eqname (Syntax.methName m) name)  
                      (Common.methods getClass ty)
         in
         let formalNames = Syntax.thisName::(Syntax.methArgs meth) in
         let args = (TmNew (dummyinfo, ty, names, terms)) :: args 
         in
                                (*dbg "args:\t%a ==>\nformals:\t%a" 
                                (Syntax.l2p Syntax.tm2s) args 
                                (Syntax.l2p Syntax.n2s) formalNames;*)
         let meth0 = meth in
         let meth = (* perform substitution in method body *) 
                    Substitution.substmult 
                    Substitution.substmethtm args formalNames meth 
         in
         let tmBody =  Syntax.methBody meth 
         in
            (*dbg "Before ty subst:\n %a\nafter tmsubst:\n%a" 
            Syntax.m2p (MemMeth meth0) 
            Syntax.m2p (MemMeth meth);*)
            tmBody
      end 
   in
   let congruenceRule x = snd (tmRedVisitor reductionRule () x) 
   in
       try
         dbgTry "Eval" (function 
           Substitution.SubstException s ->
              prc Magenta "%s\n" (Substitution.string_of_sexn s); 
           true  
          | NoRuleApplies -> true 
          | x ->
             prc Yellow "%s\n" (Printexc.to_string x)  ;
             false ) (*(fun _ -> evtm term)*)
         (fun _ -> 
           begin match Common.isClassGraphTree getClass 
                      (List.map (function DeclClass cls -> cls) decls) with
              Some(names) -> raise NoRuleApplies
            | None -> congruenceRule term
           end
         )
       with 
          Substitution.SubstException _ -> raise NoRuleApplies


