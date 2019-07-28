type iseq =
  | INil
  | IStr of string
  | IAppend of iseq * iseq

let iStr s = IStr s
let iNewline = iStr "\n"
let iAppend x y = IAppend (x, y)
let iIndent x = x

let iConcat xs = List.fold_right iAppend xs INil
let iInterleave y xs = List.fold_right (fun x z -> iAppend x (iAppend y z)) xs INil

let rec pprExpr = function
  | Expr.EVar v -> iStr v
  | Expr.EAp (e1, e2) -> iAppend (pprExpr e1) (iAppend (iStr " ") (pprExpr e2))
  | Expr.ELet (isrec, defns, expr) ->
      iConcat
        [ iStr (if isrec then "letrec" else "let")
        ; iNewline
        ; iStr " "
        ; iIndent (pprDefns defns)
        ; iNewline
        ; iStr "in "
        ; pprExpr expr
        ]
  | _ -> failwith "Not Implemented"
and pprDefns defns =
  let sep = iConcat [iStr ";"; iNewline] in
  iInterleave sep (List.map pprDefn defns)
and pprDefn (name, expr) =
  iConcat
    [ iStr name
    ; iStr " = "
    ; iIndent (pprExpr expr)
    ]
