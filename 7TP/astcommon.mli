type unop  =
  | Uminus
  | Not

type binop =
  | Plus | Minus | Mult | Div
  | And  | Or
  | Eq   | Neq   | Lt   | Le  | Gt | Ge

type const =
  | Cint  of int
  | Cbool of bool

type ty =
  | Tint
  | Tbool
  | Tarr of ty
type fty = { return_ty: ty option; params_ty: ty list }
    
