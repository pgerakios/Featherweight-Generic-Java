open Format
open Syntax
open Support.Error
open Support.Pervasive
open Set
open Printexc
open Context

exception NotImplemented
exception Impossible


(*
(* guarded "gd" execution*)
let gd fi fn = 
   begin try
      fn ()
   with 
     Context.ContextError(str) ->
       error fi str
    | Exit(n) ->
       setDebug true;
       error fi "Exit exception was caught."
    | _ ->
      setDebug true;
      error fi "Uknown error. Hopefully backtrace will help."
   end 


(* ------------------------   EVALUATION  ------------------------ *)

let checkCyclicClassGraph ctx = 
   let cls = getClasses ctx in
   let rec checkCyclicPath visited (name, _, _, supty, _) =
         let exists = 
            List.exists (fun name' ->  name' = name) visited
         in
            if exists then begin
               prlist pr visited; pr "\n";
               error dummyinfo 
               ("Found the above cyclic class definitions")
            end else begin 
               match supty with 
                 TyObject -> ()
               | TyName(name', _) ->
                     checkCyclicPath (name::visited) (getClass ctx name')
               | _ -> 
                     raise Impossible
            end

   in
      List.iter (checkCyclicPath []) cls 


(* get the upper bound of a type variable or the type it self *)
let bound ctx ty = (* get concrete type of ty*) 
   pr "BOUND OF: ";
   printty ctx ty;
   pr "\n";
   begin match ty with
       TyVar(i,j) ->  getTypeFromContext dummyinfo ctx i 
     | _ -> ty
   end 


   
(* substitute a list of types in the same order as
   the type variables at class decl*)
let substty ctx lst ty = 
 let i = ref 1 in
 List.fold_left (fun retty insty ->
    pr "Subtituting in type: " ;
    printty ctx retty;
    pr ("   " ^ (string_of_int !i) ^ " var with "); 
    printty ctx insty;
    pr "\n";
    i := !i + 1;
    typeSubstTop insty retty)  ty lst

let substtmty lst tm = (* substitute a list of types in a term *) 
 List.fold_left (fun tm insty -> tytermSubstTop insty tm)  tm lst


let rec fields ctx ty = (* must be concrete ty *)
   let ty = bound ctx ty in
   begin match ty with 
      | TyObject -> []
      | TyName(name, tyargs) -> 
         let (_, tvars, _, supty, members) = getClass ctx name in   
         let flds = List.fold_left (fun fields member -> 
               begin match member with 
                | MemField(_, name, ty) -> 
                      let exists = 
                         List.exists (fun (name', _) -> name = name') fields
                      in (* check duplicate fields*)
                         if exists then 
                            error dummyinfo "Found duplicate field name." 
                         else 
                           (name, ty):: fields
                | _ -> fields
               end) [] members 
         in
         let flds' = 
            fields ctx (substty ctx tyargs supty) (* substitute type and get fields*)
         in
            List.iter (fun (name, _) -> 
               List.iter (fun (name', _) -> 
                  if name = name' then 
                     error dummyinfo "Duplicate field name in class hierarchy"
            ) flds ) flds';

            List.append flds flds'

      | TyVar _ -> raise Impossible
   end 


(* Get a list of class methods as well as the methods of 
 * the super classes ordered by reverse hierachy order *)
let rec methods ctx ty = (* must be concrete ty *)
   let ty = bound ctx ty in
   begin match ty with 
      | TyObject -> []
      | TyName(name, tyargs) -> 
         let (clsName, _, _, supty, members) = getClass ctx name in   
         let meths = List.fold_left (fun meths member -> 
               begin match member with 
                | MemMeth (fi, name, argtys, retty, body) -> 
                  (* perform type substitution in the entire method *)
                  let ((fi, name, argtys, retty, body) as  meth) = 
                     (fi, name, List.map (fun (arg,ty) -> (arg, substty ctx tyargs
                     ty)) argtys, substty ctx tyargs retty,  substtmty tyargs body )
                  in
                  let sameName = 
                         List.exists (fun (_, name', _, _, _) -> name = name') meths
                  in (* check duplicate methods *)
                         if sameName then 
                            error dummyinfo "Found duplicate method name." 
                         else 
                            (fi, name, argtys, retty, body) :: meths 
                | _ -> meths
               end) [] members 
         in
         let meths' = 
              methods ctx (substty ctx tyargs supty) (* substitute type and get fields*)
         in
         let meths'' = List.append meths meths' (*merge my methods with methods
         of hierarchy *)
         in
           (*iterate over merged methods and verify that each method group
            * consisting of methods having the same name also have the same 
            * type signature *)
         let allOK = 
           List.for_all (fun (fi, name, argtys, retty, body) -> 
              let sameName = 
                 List.filter (fun (_, name', _, _, _) -> name = name') meths''
              in
              let tys = retty :: (List.map snd argtys) in
              let allOK = 
                 List.for_all (fun (fi, name', argtys', retty', _) -> 
                                 let tys' = retty' :: (List.map snd argtys') in
                                 (List.length tys) = (List.length tys') &&
                                 (List.for_all 
                                 (fun (ty1, ty2) -> eq_ty ctx ty1 ty2) 
                                 (List.combine tys tys'))
                              ) sameName
              in
                 allOK
             ) meths''

          in 
             if not allOK then
                Support.Error.error dummyinfo "Found methods in hierarchy that have the same name but
                do not the same type signature."
              else 
                 meths''

      | TyVar _ -> raise Impossible
   end 


and mtype ctx name ty = 
   let all = methods ctx ty 
   in
      try
         let (_, _, argtys, retty, _) =
            List.find (fun (_, name', _, _, _)-> name = name') all
         in
            (List.map snd argtys, retty)
      with Not_found ->
         error dummyinfo ("Could not find type of method " ^ name ^ ". Does it  exist?") 


and wf_ty ctx ty = ()  (* TODO *)
and tc_term ctx tm = 
 begin match tm with 
    TmVar(fi, i, j) ->  getTypeFromContext fi ctx i
  | TmThis fi ->  getTypeFromContext fi ctx (name2index fi ctx "this")
  | TmNew(fi, ty, names, args) -> 
      wf_ty ctx ty; (* type of new must be well-formed *)

      let flds = fields ctx ty in 
         if (List.length flds) != (List.length names) then 
            error fi "Mismatch in the number of named arguments and fields of this type"
         else begin
          let namedArgs = List.combine names args in 
          if not (List.for_all (* for all fields in hierarchy there must exist an
           assignment for this field with the correct subtype *)
           (fun (fld, fty) -> 
              let ret  = List.exists 
               (fun (n, tm) ->
                 if fld <> n then false 
                 else if sub_ty ctx (tc_term ctx tm) fty then(* assigned value must be a subtype*)
                   true
                 else 
                    error fi "Attempted to assign a field a value that is not a subtype of its type."
               ) namedArgs
              in
                if not ret then
                   pr ("Could not find field " ^ fld ^ " in [" );
                   prlist (fun s -> pr (fst s)) namedArgs;
                   pr( "]\n");
                ret 
           ) flds) then
               error fi "Did not include all fields in type constructor"
           else
               pr "tcterm : "; 
               printty ctx ty; 
               pr "\n";
               ty 
         end

  | TmInvoke(fi, t1, meth, args) -> 
   let (nominal, cname, tyargs) = (* get class type and instantiation type args*) 
      begin match bound ctx (tc_term ctx t1)  with 
       | TyName(cname, tyargs) as nominal -> (nominal, cname, tyargs)
       | _ -> error fi "Expected concrete type" 
      end
   in
   let (argsty, retty) =  mtype ctx meth (tc_term ctx t1) in (* get method signature*)
   let argsty' = List.map (tc_term ctx) args in 

   if (List.length argsty) != (List.length argsty') ||
       (List.exists (fun (ty', ty) -> not (sub_ty ctx ty' ty)) 
       (List.combine argsty' argsty)) 
   then 
        error fi "Some of the argument types are not subtypes of the formal parameters or invalid number of arguments."
   else (* ok argument types are subtypes and equal to the number of formal arguments*)
         retty

  | TmField(fi, t1, name) ->
        begin match bound ctx (tc_term ctx t1)  with 
          TyName(cname, tyargs) as nominal ->
           begin try
               let _, ty = 
                  List.find (fun (fld, _) -> name = fld) (fields ctx nominal)
               in
                  ty
            with Not_found -> 
               error fi "Invalid field name."
            end
         | _ -> 
               error fi "Expected a concrete type here. Cannot access field here."
        end

 end


and eq_ty ctx ty1 ty2 =  (sub_ty ctx ty1 ty2) && (sub_ty ctx ty2 ty1)

and sub_ty ctx ty1 ty2 = (ty2 = TyObject) ||
   (ty1 != TyObject &&
   (let ty1 = bound ctx ty1 in
    let ty2 = bound ctx ty2 in
      match ty1,ty2 with 
        (TyName (n1, l1), TyName (n2, l2)) ->
           if n1 = n2 then 
              (List.length l1) = (List.length l2) && 
              (List.for_all (fun (ty1, ty2) -> eq_ty ctx ty1 ty2)  (List.combine l1 l2))
           else begin (* Not the same type *)
             let (_, _, _, supty, _) = getClass ctx n1 in
             pr ("\n class " ^ n1 ^ " with "); 
             pr " supty orig: ";
             printty ctx supty;
             pr "\n type sub list : ";
             prlist (printty ctx) l1;
             pr "\n resulting type : ";
             let supty = substty ctx l1 supty (* instantiate super type with actuals *)
             in
                printty ctx supty;
                pr "\n";

                sub_ty ctx supty ty2
           end 
      | _ -> raise Impossible 
           
   ))

let tc_member ctx clsName member = 
   begin match member with 
      MemField(fi, name, ty) -> 
         wf_ty ctx ty (* is field type well formed ?*)
    | MemMeth(fi, methName, typedArgs, retty, body) -> 
      let ctx = var2ctx ctx typedArgs in
      let (_, tvars, _, _, _) = getClass ctx clsName in
      let clsType = 
            TyName (clsName, List.map 
            (fun tv -> 
               let i  = name2index dummyinfo ctx tv
               in 
                  getTypeFromContext dummyinfo ctx i 
            ) tvars)
      in
      let ctx = addbinding ctx "this" (TyVarBind clsType) in (* add type of "this" *)

      List.iter (fun (_, ty) -> wf_ty ctx ty) typedArgs; (*arg type well-formedness*)
      wf_ty ctx retty; (* return type well-formedness *)
      let retty' = tc_term ctx body in (* type check method body*)

      if not (sub_ty ctx retty' retty) then (* returned type must be a subtype of declared return  type*)
      begin
        (*print_break  0 0;
        print_break  0 0;*)
        pr "Actual return type is ";
        printty ctx retty'; pr " but expected the return type to be a subtype of the declared return type ";
        printty ctx retty; pr "\n";
        error fi ("\nError while checking method " ^ clsName ^ "." ^  methName ^ "\n")
      end
   end

(* type-check class declaration *)
let tc_cls clsenv ((name, tvarNames, constraints, superty, members)  as cls) =
  let ctx = tvar2ctx clsenv tvarNames constraints in (* add subtyping constraints to clsEnv *)

  let clsType = TyName(name, []) in (* no substitutions are required *) 
  (* check there are no duplicate fields in class hierarchy*)
  let _ = fields ctx clsType in 

  pr ("Type checking class " ^ name ^ "\n");
  onDebug (fun _ -> prcls clsenv cls; pr "\n");

  wf_ty ctx superty; (* is super type well formed ? *) 
  List.iter (fun (sub, sup) -> 
     wf_ty ctx sub;
     wf_ty ctx sup) constraints; (* well-formedness of tvars and their supertypes *)
  List.iter (tc_member ctx name) members (* member well-formedness *)

(* type check all declarations in a program *)
let tc_decls ctx =  
   let cls = getClasses ctx in
      onDebug (fun _ ->
         pr ("Context length: " ^ (string_of_int (ctxlength ctx)) ^ "\n");
         pr ("Classes found: " ^ (string_of_int (List.length cls)) ^ "\n\n");
         prctx "MAIN" ctx
      );
      checkCyclicClassGraph ctx; (* rule out cyclic class hierarchies *)
      List.iter (tc_cls ctx) cls;
      pr "Type checking declarations [OK]"



(* typecheck a term, evaluate it and repeat until it becomes a value *)
let eval ctx t1 =
   tc_decls ctx; (* type-check all declarations*)
   onDebug( fun _ ->      
      pr "Term : ";
      printtm ctx t1;
      pr " ::: "
   );
   let t1ty = tc_term ctx t1 (* type-check term*)
   in 
      onDebug (fun _ ->  
        printty ctx t1ty;
        pr "\n");
       (* TODO: evaluate and type-check each redex*)
      raise NotImplemented
*)
let eval ctx t1 = raise NotImplemented
let typeof tr1 = raise NotImplemented
let subtype ty ty = raise NotImplemented
let isval t1 = raise NotImplemented

