type name = string
type is_rec = bool

type 'a expr =
  | EVar of name
  | ENum of int
  | EConstr of int * int
  | EAp of ('a expr) * ('a expr)
  | ELet of is_rec * ('a * 'a expr) list * 'a expr
  | ECase of 'a expr * ('a alter) list
  | ELam of ('a list * 'a expr)
and 'a alter = (int * 'a list * 'a expr)

let binders_of x = List.map fst x
let rhss_of x = List.map snd x

let is_atomic = function
  | EVar _ -> true
  | ENum _ -> true
  | _ -> false

type core_expr = name expr
type core_alt = name alter

type 'a sc_defn = name * 'a list * 'a expr 
type core_sc_defn = name sc_defn

type 'a program = 'a sc_defn list
type core_program = name program
