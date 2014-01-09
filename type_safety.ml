open Syntax
open Support.Error
open Support.Pervasive

exception NotImplemented
exception Impossible
exception ViolProgress of string 
exception ViolPreservation of string 

let rsprog s = raise (ViolProgress s)
let rspres s = raise (ViolPreservation s)


let unsafe = ref false 
let setUnsafe b = unsafe := b

let runSafety decls term =
 let ctx =  if !unsafe then Typecheck.emptyContext
            else Typecheck.tcDecls decls 
 in (* check that all declarations are well-typed *)
 let subtype a b =   (!unsafe) || 
    (Typecheck.subTy ctx a b)
 in (* find subtypes in top-level context *) 
 let typeOf step t =  (* type-check term in top-level context *)
    info "Running step : %d\nTerm: %a\n" step Syntax.tm2p t;
   if !unsafe then
   begin
      prc Green  " ---> \n";
      TyObject
   end
   else begin
    let ty = Typecheck.typeOf ctx t (* term is well-typed with type ty*)
    in
       prc Magenta " :: %a\n\n ---> " Syntax.ty2p ty;
       ty
   end
 in 
 let rec run step t ty =   
    let t' = Eval.eval decls t in (* perform a step *)
    let ty' = typeOf step t' in (* type check new term*)
    if subtype ty' ty then
    begin   
      if Eval.isval t' then (* there are have no more steps *) 
      begin
         onDebug(fun _ -> prc Yellow "\nThe term has been reduced to a value.\n");
         (t', ty')
      end
      else
       run (step+1) t' ty' (* perform more steps*)
    end
    else 
       begin
          rspres "PRESERVATION VIOLATION! Term was reduced to something that is not a subtype of the original type." 
       end

 in
   let (tm, ty) = 
      begin try
         dbgTry "Type_safety" ((=) Eval.NoRuleApplies)
         (fun _ ->
            let ty = (typeOf 0 term) 
            in
            if Eval.isval term then (term, ty)
            (* evaluate term only if a step can be performed *)
            else run 1 term ty) 
      with 
         Eval.NoRuleApplies ->
            rsprog "PROGRESS VIOLATION! Term could not be reduced to
           a value. Invalid term."
      end
   in 
    prc Red "\n\n Result:\t %a\tis a value" Syntax.tm2p tm;
    if not (!unsafe) then
          prc Green " with type %a\n" Syntax.ty2p ty
    else pr "\n"
