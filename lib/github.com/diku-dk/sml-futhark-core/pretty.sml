fun intersperse y [] = []
  | intersperse y [x] = [x]
  | intersperse y (x :: xs) =
      x :: y :: intersperse y xs

fun punctuate c = concat o intersperse c

fun prettySubExp se = se

fun prettyType UNIT = "unit"
  | prettyType BOOL = "bool"
  | prettyType I8 = "i8"
  | prettyType I16 = "i16"
  | prettyType I32 = "i32"
  | prettyType I64 = "i64"
  | prettyType F16 = "f16"
  | prettyType F32 = "f32"
  | prettyType F64 = "f64"
  | prettyType (ARRAY (d, t)) =
      "[" ^ prettySubExp d ^ "]" ^ prettyType t

fun prettyRetType (UNIQUE t) = "*" ^ prettyType t
  | prettyRetType (NONUNIQUE t) = prettyType t

fun prettyRetAls (RETALS (pals, rals)) =
  "("
  ^
  punctuate ", "
    [ "[" ^ punctuate "," (map Int.toString pals) ^ "]"
    , "[" ^ punctuate "," (map Int.toString rals) ^ "]"
    ]

fun prettyRet ret =
  "{"
  ^
  punctuate ", "
    (map (fn (t, als) => prettyRetType t ^ "#" ^ prettyRetAls als) ret) ^ "}"

fun prettyParam (v, t) = v ^ ": " ^ prettyType t

fun prettyPat (PAT pes) =
  "{" ^ punctuate ", " (map prettyParam pes) ^ "}"

fun prettyExp (SUBEXP se) = se
  | prettyExp (BINOP (f, (x, y))) =
      f ^ "(" ^ x ^ ", " ^ y ^ ")"
  | prettyExp (APPLY (f, args, ret)) =
      "apply " ^ f ^ "(" ^ punctuate ", " (map prettySubExp args) ^ ")" ^ " : "
      ^ prettyRet ret
  | prettyExp (SOAC soac) = prettySoac soac

and prettyStm (STM (pat, exp)) =
  "let " ^ prettyPat pat ^ " = " ^ prettyExp exp

and prettyBody (BODY (stms, res)) =
  "{\n"
  ^
  punctuate "\n"
    (map prettyStm stms @ ["in {" ^ punctuate ", " (map prettySubExp res) ^ "}"])
  ^ "\n}"

and prettySoac (MAP (w, arrs, lam)) =
  "map(" ^ prettySubExp w ^ ", {" ^ punctuate "," arrs ^ "}, "
  ^ prettyLambda lam ^ ")"

and prettyLambda (LAMBDA (params, types, body)) =
  "\\ {" ^ punctuate " " (map prettyParam params) ^ "} : {"
  ^ punctuate ", " (map prettyType types) ^ "} -> " ^ prettyBody body

fun prettyFunDef (FUNDEF {name, params, ret, body}) =
  "fun " ^ name ^ "(" ^ (punctuate ", " (map prettyParam params)) ^ ")" ^ "\n"
  ^ ": " ^ prettyRet ret ^ " = " ^ prettyBody body

fun prettyProg (PROG ((), stms, funs)) =
  punctuate "\n" (map prettyStm stms @ map prettyFunDef funs)
