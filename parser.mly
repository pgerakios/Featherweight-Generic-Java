/*  
 *  Yacc grammar for the parser.  The files parser.mli and parser.ml
 *  are generated automatically from parser.mly.
 */

%{
open Support.Error
open Support.Pervasive
open Syntax
%}

/* ---------------------------------------------------------------------- */
/* Preliminaries */

/* We first list all the tokens mentioned in the parsing rules
   below.  The names of the tokens are common to the parser and the
   generated lexical analyzer.  Each token is annotated with the type
   of data that it carries; normally, this is just file information
   (which is used by the parser to annotate the abstract syntax trees
   that it constructs), but sometimes -- in the case of identifiers and
   constant values -- more information is provided.
 */

/* Keyword tokens */
%token <Support.Error.info> LAMBDA
%token <Support.Error.info> IF
%token <Support.Error.info> THEN
%token <Support.Error.info> ELSE
%token <Support.Error.info> TRUE
%token <Support.Error.info> FALSE
%token <Support.Error.info> BOOL
%token <Support.Error.info> LET
%token <Support.Error.info> FOR
%token <Support.Error.info> IN

%token <Support.Error.info> EMPTY
%token <Support.Error.info> UNIT
%token <Support.Error.info> FLOAT
%token <Support.Error.info> INT

%token <Support.Error.info> OBJECT
%token <Support.Error.info> CLASS
%token <Support.Error.info> THIS
%token <Support.Error.info> RETURN
%token <Support.Error.info> NEW
%token <Support.Error.info> EXTENDS
%token <Support.Error.info> SUPER

%token <Support.Error.info> TTOP
%token <Support.Error.info> TBOT
%token <Support.Error.info> TBOOL
%token <Support.Error.info> TTRUE
%token <Support.Error.info> TFALSE
%token <Support.Error.info> TRECORD
%token <Support.Error.info> TBASE
%token <Support.Error.info> TLANG
%token <Support.Error.info> TLABEL



/* Identifier and constant value tokens */
%token <string Support.Error.withinfo> UCID  /* uppercase-initial */
%token <string Support.Error.withinfo> LCID  /* lowercase/symbolic-initial */
%token <string Support.Error.withinfo> TCID  /* lowercase/symbolic-initial */
%token <string Support.Error.withinfo> BCID  /* lowercase/symbolic-initial */
%token <int Support.Error.withinfo> INTV
%token <float Support.Error.withinfo> FLOATV
%token <string Support.Error.withinfo> STRINGV

/* Symbolic tokens */
%token <Support.Error.info> APOSTROPHE
%token <Support.Error.info> DQUOTE
%token <Support.Error.info> ARROW
%token <Support.Error.info> BANG
%token <Support.Error.info> BARGT
%token <Support.Error.info> BARRCURLY
%token <Support.Error.info> BARRSQUARE
%token <Support.Error.info> COLON
%token <Support.Error.info> COLONCOLON
%token <Support.Error.info> COLONEQ
%token <Support.Error.info> COLONHASH
%token <Support.Error.info> COMMA
%token <Support.Error.info> DARROW
%token <Support.Error.info> DDARROW
%token <Support.Error.info> DOT
%token <Support.Error.info> EOF
%token <Support.Error.info> EQ
%token <Support.Error.info> EQEQ
%token <Support.Error.info> EXISTS
%token <Support.Error.info> GT
%token <Support.Error.info> HASH
%token <Support.Error.info> LCURLY
%token <Support.Error.info> LCURLYBAR
%token <Support.Error.info> LEFTARROW
%token <Support.Error.info> LPAREN
%token <Support.Error.info> LSQUARE
%token <Support.Error.info> LSQUAREBAR
%token <Support.Error.info> LT
%token <Support.Error.info> RCURLY
%token <Support.Error.info> RPAREN
%token <Support.Error.info> RSQUARE
%token <Support.Error.info> SEMI
%token <Support.Error.info> SLASH
%token <Support.Error.info> STAR
%token <Support.Error.info> PLUS
%token <Support.Error.info> TRIANGLE
%token <Support.Error.info> USCORE
%token <Support.Error.info> VBAR
%token <Support.Error.info> SEMISEMI

/* ---------------------------------------------------------------------- */
/* The starting production of the generated parser is the syntactic class
   toplevel.  The type that is returned when a toplevel is recognized is
     Syntax.context -> (Syntax.command list * Syntax.context) 
   that is, the parser returns to the user program a function that,
   when given a naming context, returns a fully parsed list of
   Syntax.commands and the new naming context that results when
   all the names bound in these commands are defined.

   All of the syntactic productions in the parser follow the same pattern:
   they take a context as argument and return a fully parsed abstract
   syntax tree (and, if they involve any constructs that bind variables
   in some following phrase, a new context).
   
*/

%start toplevel
%type < Syntax.names -> Syntax.prog > toplevel
%%

/* ---------------------------------------------------------------------- */
/* Main body of the parser definition */

/* The top level of a file is a sequence of commands, each terminated
   by a semicolon. */
toplevel :
  DeclarationList TermList EOF  
      { fun _ -> 
            let tvars = emptyNames in 
            ($1 tvars, $2 tvars) 
      }

TermList:       
   Term SEMI
 { fun names -> [$1 names] }
|  Term SEMI TermList
{ fun names -> ($1 names)::($3 names) } 

/* A top-level command */
DeclarationList:
    /* empty */
      { fun names -> [] }

 |  CLASS UCID TVarListOpt EXTENDS AType 
    LCURLY MemberList RCURLY SEMI DeclarationList
      {
        fun env_tvars ->
           let name = mkname $2 in
           let (tvars, mkcons) = $3 env_tvars in
           let tycons =  
              List.map (fun mktyc -> mktyc tvars) mkcons 
           in
           let supty = $5 tvars in
           let members = $7 [] [] tvars in
           let decls = $10 env_tvars 
           in
              (DeclClass (name, tycons, supty, members)) :: decls
      }

TVarListOpt:      
    /* empty */
      { fun _ ->  (emptyNames, []) }
| LT ConstraintList  GT
      { fun names ->  $2 names }

ConstraintList: 
   UCID EXTENDS AType 
   {
      fun names -> 
         let name = mkname $1 in
         let names =  addName name names in
         let mkcons names =
            (ConSubtype (name, $3 names))
         in 
            (names, mkcons::[])
   }
 | UCID EXTENDS AType COMMA ConstraintList
   {
      fun names->
         let name = mkname $1 in
         let names =  addName name names in
         let mkcons names = (* generate names at a later time*)
            (ConSubtype (name, $3 names))
         in 
         let (names, mkconstail) = $5 names 
         in 
            (names, mkcons::mkconstail)
   }

/* All type expressions */

/* Atomic types are those that never need extra parentheses */
AType :
   OBJECT
      { fun _ -> TyObject }
  | UCID InstantiationListOpt
      { fun names -> 
         let tys = $2 names in 
         let name = mkname $1 in
         let c1 = tys = [] in
         let c2 = Syntax.existsName name names in
         (*dbg "Tyvar %a (%d) ? %b c1=%b c2=%b names=%a.\n"
         Syntax.n2p name (Syntax.getNameScope name) 
         (c1 && c2) c1 c2
         Syntax.ns2p names;*)
         if c1 && c2 then
            TyVar  (Syntax.mktyvar name)
         else 
            TyName (Syntax.mktyname name tys)
      }


InstantiationListOpt:                
   /* empty */
      { fun _  -> [] }

  | LT InstantiationList GT
      { fun names -> $2 names }

InstantiationList:
   AType
   {
      fun names -> ($1 names)::[]
   }
 | AType COMMA InstantiationList
   { 
     fun names -> ($1 names)::($3 names)
   }


FormalArgList:     
    /* empty */
      { 
         fun _ -> 
            ([], emptyNames) 
      }
   | AType LCID
      {
         fun names ->
            let ty = $1 names in
            let name = mkname $2 in  
            let typedArg = (name, ty) in
            let names = addName name names in
            (typedArg :: [], names)
      }
   | AType LCID COMMA FormalArgList
      {  
         fun names ->
            let ty = $1 names in 
            let name = mkname $2 in
            let typedArg = (name, ty) in
            let names' = addName name names in
            let tail, names''  =  $4 names' in
            (typedArg :: tail, names'')
      }


MemberList:
   /* empty */
      { fun _  _ _ -> [] }
   | AType LCID LPAREN FormalArgList RPAREN LCURLY RETURN Term SEMI RCURLY SEMI MemberList
      {  
        fun methodNames fieldNames classCtx -> 

           if List.exists (fun name -> $2.v = name) methodNames then
              error $2.i "Duplicate method declaration. No overloading is allowed.";

           let retty = $1 classCtx in
           let formals, classCtx'' = $4 classCtx in
           let tmbody = $8 classCtx'' in
           let memberTail = $12 ($2.v :: methodNames) fieldNames classCtx in
            (MemMeth (mkname $2, formals, retty, tmbody)) :: memberTail 
      }
   | AType LCID SEMI MemberList
      {  
         fun methodNames fieldNames classCtx ->

           if List.exists (fun name -> $2.v = name) fieldNames then
              error $2.i "Duplicate field declaration.";
 
           let retty = $1 classCtx in
           let memberTail = $4 methodNames ($2.v :: fieldNames) classCtx in
           (MemField (mkname $2, retty)) :: memberTail 
      }


InitList:     
    /* empty */
      { fun _ -> [] }
   | LCID EQ Term
      {
        fun names -> (mkname $1, $3 names)::[]
      }
   | LCID EQ Term COMMA InitList
      { 
        fun names ->
           let tl = $5 names in
           let exists = 
              List.exists (fun (n, _) -> $1.v = string_of_name n) tl
           in
               if exists then
                  error $1.i "Duplicate field name in named argument list."
               else
                  (mkname $1, $3 names)::tl
      }

ArgList:     
    /* empty */
      { fun _ -> [] }
   |  Term
      {
         fun names -> ($1 names) :: [] 
      }
   |  Term COMMA ArgList
      {  
         fun names ->  ($1 names) :: ($3 names)
      }

ArgListOpt:
    /* empty */
      { fun _ -> None }
|   LPAREN ArgList RPAREN
      { fun names -> Some ($2 names) }

Term:    
    ATerm
      { fun names -> $1 names } 
  | ATerm DOT LCID ArgListOpt
      { fun names ->
          let  tm = $1 names in
          let  name = mkname $3 in 
          match $4 names with
             None -> TmField($3.i, tm, name)
           | Some(args) -> TmInvoke($3.i, tm, name, args)
      }
  | NEW AType LPAREN InitList RPAREN
      { 
         fun names -> 
             let ty = $2 names in
             let names, tms = List.split ($4 names) in
                TmNew ($1, ty, names, tms)
      }


ATerm :
    LPAREN Term RPAREN  
      { fun names -> $2 names }
  | THIS
      { fun _ -> TmVar thisName }
  | LCID 
    { fun _ -> TmVar(mkname $1) }
