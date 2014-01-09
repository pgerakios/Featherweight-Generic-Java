open Format
open Support.Error
open Support.Pervasive
open Context
(* ---------------------------------------------------------------------- *)
(* Datatypes *)

(* information about the variable
 * scope 
 * variable name  
 * *)
type name = (info * int * string)


(* Data type definitions *)
type ty =
    TyObject
  | TyName of tyname 
  | TyVar  of tyvar

and  tyname = name * (ty list)
and  tyvar = name 

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
(*type cls = name * (name list) * ((ty * ty) list) * ty * (member list)*)
type cls = name * tycons * ty * (member list)

type decl =
   DeclClass of cls 

type decls = decl list
type terms = term list
type prog = decls * terms
(*----------------------------------------------------------------------------------------------*)

let eqlist (cmp:'a->'b->bool) (l1:'a list) (l2: 'b list) = 
   ((List.length l1) = (List.length l2)) && 
   (List.for_all (fun (x,y) -> cmp x y) (List.combine l1 l2))

   (* generic list to string conversion *)
let rec al2s : string -> ('a -> string) -> 'a list -> string  =
  fun sep fn a -> 
    begin match a with
      [] -> ""
     | hd::[] -> fn hd
     | hd::tl -> strOf "%a%s%a" (fun () -> fn) hd sep (fun ()-> al2s sep fn) tl
   end


let al2p (sep:string) (fn : ('a -> string)) () (a : 'a list) : string = 
 al2s sep fn a 

let l2s :  'a. ('a -> string) -> 'a list -> string =
  fun fn a ->  al2s ", " fn a  
let l2p :  'a. ('a -> string) -> unit -> 'a list -> string = 
   (fun fn () -> l2s fn)


(*
type binding =
    NameBind
  | ClassBind of cls   (* class definition for some class "Foo" *) 
  | TyVarBind of ty    (* upper bound of type variable "X" *)
  | VarBind of ty        (* type of some variable "x"    *)



type context = (string * binding) list
*)

(* ---------------------------------------------------------------------- *)
(* tyname API *)
let tyname2name (n, _) = n
let tyname2tyargs (_, a) = a
let mktyname n tys = n, tys
(* ---------------------------------------------------------------------- *)

(* tyvar API *)
let tyvar2name x = x
let mktyvar x = x

(* ---------------------------------------------------------------------- *)

let mkname (v:(string Support.Error.withinfo))  = v.i, 0, v.v

let string_of_name (_, _, n) = n

let name2info (i, _, _) = i

let eqname (_, n1, s1) (_, n2, s2) =   
   let c1 = s1 = s2 in
   let c2 = n1 = n2 in
   let ret = c1 && c2 
   in 
      (*dbg "Compare %s (%d) in %s (%d)? %b.\n" s1 n1 s2 n2 ret;*)
      ret

let cmpname ((_, n1, s1):name) ((_, n2, s2):name) : int = 
     Pervasives.compare (n1,s1) (n2,s2)


let thisName = dummyinfo, 0, "this"
(* ----------------------- set of names --------------------------------  *)
module Name = struct 
  type t = name
  let compare = cmpname
end

module NameSet = Set.Make (Name)

type names = NameSet.t

exception NameExists of (name * names) 

let ns2s n = l2s (fun (_, d, s) -> strOf "%s (%d)" s d)  (NameSet.elements n)
let ns2p () = ns2s

let addName name names = 
   if NameSet.mem name names then 
      raise (NameExists (name, names))
   else 
      NameSet.add name names

let existsName name names =
   (*let (_, n, s) = name in *)
   let ret = NameSet.mem name names 
   in 
         (*dbg "Exists %s (%d) in %a? %b.\n" s n         
         ns2p names ret;
         *)
       ret
 

let tycons2names tc = 
  let tc2n t = 
     match t with 
        ConSubtype(n, _) -> n
   in 
      List.fold_left (fun names t -> NameSet.add (tc2n t) names) NameSet.empty tc

let emptyNames = NameSet.empty

(* ---------------------------------------------------------------------- *)
(* Extracting file info *)

let rec tmInfo t = match t with
    TmVar n -> name2info n
  | TmNew(fi, _, _, _) -> fi
  | TmInvoke(fi, _, _, _) -> fi
  | TmField(fi, _, _) -> fi

(* ---------------------------------------------------------------------- *)
(* Printing *)

(* The printing functions call these utility functions to insert grouping
  information and line-breaking hints for the pretty-printing library:
     obox   Open a "box" whose contents will be indented by two spaces if
            the whole box cannot fit on the current line
     obox0  Same but indent continuation lines to the same column as the
            beginning of the box rather than 2 more columns to the right
     cbox   Close the current box
     break  Insert a breakpoint indicating where the line maybe broken if
            necessary.
  See the documentation for the Format module in the OCaml library for
  more details. 
*)


let obox0() = open_hvbox 0
let obox() = open_hvbox 2
let cbox() = close_box()
let break() = print_break 0 0

open Printf
let rec n2p () name = (string_of_name name) 
and n2s = string_of_name 
and tm2p () = tm2s
and ty2p () = ty2s
and tm2s (tm:term) : string =
 let rec f tm =
   begin match tm with
     TmVar n -> strOf "%a" n2p n 
   | TmNew(i, ty, names, tms) ->
        strOf "new %a(%a)" ty2p ty  
        (l2p (fun (n, tm) -> strOf "%a =  %a" n2p n tm2p tm)) 
        (List.combine names tms)
   | TmInvoke (i, tm, name, tms) ->
        strOf "%a.%a(%a)" tm2p tm n2p name (l2p tm2s) (tms:term list)
   | TmField(i, tm, name) -> 
        strOf "%a.%a" tm2p tm n2p name 
   end
 in 
    f tm
and ty2s (ty:ty) = 
  let rec f ty =
     begin match ty with 
         TyObject -> "Object"
       | TyName (name, tys) ->  
             strOf "%a<%a>" n2p name (l2p ty2s) tys
       | TyVar name ->  strOf "%a" n2p name 
     end
  in
     f ty
and m2s = function 
     MemMeth (n, argTys, retTy, tmBody) ->
        strOf "%a %a(%a){\n\treturn %a;\n};" ty2p retTy n2p n
        (l2p (fun (n, ty) -> strOf "%a %a" ty2p ty n2p n)) argTys 
        tm2p tmBody         
   | MemField(n, ty) -> strOf "%a %a" ty2p ty n2p n
and m2p () = m2s 
and tc2s = function
   ConSubtype (n, ty) -> strOf "%a extends %a" n2p n ty2p ty
and tc2p () = tc2s
and c2p () = c2s
and c2s ((name, tycons, pty, members):cls) = 
   strOf "class %a<%a> extends %a {\n%a\n};" n2p name (al2p "," tc2s) tycons
   ty2p pty (al2p "\n" m2s) members


let printty  tyT = pr "%s" (ty2s tyT) 

let printtm  t = pr "%s" (tm2s t)


let rec prlist fn l =
   begin match l with
      [] -> () 
     | hd::[] -> fn hd
     | hd::tl -> fn hd; pr "%s" "," ; prlist fn tl
   end

let prcls cls = pr "%s" (c2s cls) 
(*-----------------------------------------------------------------------------------------------*)

      
let rec eqty ty1 ty2 = 
 begin match ty1, ty2 with 
     (TyObject, TyObject) -> true
   | (TyVar n1, TyVar n2) when (eqname n1 n2) -> true
   | (TyName (n1, tys1), TyName(n2, tys2)) ->
         (eqname n1 n2) && (eqlist eqty (tys1:ty list) tys2) 
  | _ -> 
      false 
 end


let eqmeth (methName1, ntys1, ty1, _) (methName2, ntys2, ty2, _) = 
   (eqname methName1 methName2) && 
   (eqlist eqty (ty1::(List.map snd ntys1)) (ty2::(List.map snd ntys2)))
   
let eqfld (fldName1, ty1) (fldName2, ty2) = (eqname fldName1 fldName2) && (eqty ty1 ty2)

let eqmember m1 m2 = 
   begin match m1, m2 with 
     | (MemField f1, MemField f2) -> eqfld f1 f2
     | (MemMeth m1, MemMeth m2) -> eqmeth m1 m2
     | _ -> false
   end
     
let isField m = 
  begin match m with 
       MemField _ -> true
     | _ -> false 
  end  

let isMethod m = 
  begin match m with 
       MemMeth _ -> true
     | _ -> false 
  end  

let methBody (_, _, _, tm ) = tm

let memberNameType = begin function
     | MemField (n, ty) -> (n,[ty])
     | MemMeth (n, tys1, ty1, _) -> (n, ty1::(List.map snd tys1))
   end

(*let clsArgs cls = [] *)

let clsTyArgs (_, tyc, _, _) =
      List.map (fun tyc ->
      begin match tyc with 
       ConSubtype (name, _) -> name
      end) tyc

let clsName (n, _, _, _) = n

let clsSuperTy (_, _, ty, _) = ty
let clsMembers (_, _, _, m) = m

let methArgs (_, ntys1, _, _) = List.map fst ntys1

let methArgTys (_, ntys1, _, _) = List.map snd ntys1

let methTys (_, ntys1, retTy, _) = retTy::(List.map snd ntys1)

let filtmap fn a  = 
   let rec f x = 
      begin match x with 
         [] -> []
       | hd::tl -> 
         let tl = f tl in
         begin match fn hd with
            Some b -> b::tl
          | None -> tl
         end
      end
   in
      f a
(*let methTyArgs (_, _, _, _) = []*)
let member2meth m = begin match m with
     | MemField _ -> None
     | (MemMeth m) -> Some m
   end


let member2fld  m = begin match m with 
     | MemMeth _ -> None
     | (MemField f) -> Some f
   end

let methName (n, _, _, _) = n

let fldName  (n, _) = n


(*-----------------------------------------------------------------------------------------------*)
let getNameScope ((_, s, _):name) = s
let setNameScope ((i, _, n):name) s = i, s, n
(*-----------------------------------------------------------------------------------------------*)
