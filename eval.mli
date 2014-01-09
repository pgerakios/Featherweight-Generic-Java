open Syntax

exception NoRuleApplies

val eval :   decls -> term -> term
val isval :  term -> bool
