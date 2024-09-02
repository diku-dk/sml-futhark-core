type SubExp = string

datatype Type =
  UNIT
| BOOL
| I8
| I16
| I32
| I64
| F16
| F32
| F64
| ARRAY of SubExp * Type

datatype RetType = UNIQUE of Type | NONUNIQUE of Type

datatype Pat = PAT of (string * Type) list

type Param = string * Type

type Result = string list

datatype Exp =
  SUBEXP of SubExp
| BINOP of string * (SubExp * SubExp)
| APPLY of string * SubExp list * Type list
| SOAC of Soac

and Stm =
  STM of Pat * Exp

and Lambda =
  LAMBDA of Param list * Type list * Body

and Soac =
  MAP of SubExp * string list * Lambda

and Body =
  BODY of Stm list * Result

datatype Fundef =
  FUNDEF of
    {name: string, params: Param list, rettype: RetType list, body: Body}

datatype Prog = PROG of unit * Stm list * Fundef list

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

fun prettyParam (v, t) = v ^ ": " ^ prettyType t

fun prettyPat (PAT pes) =
  "{" ^ punctuate ", " (map prettyParam pes) ^ "}"

fun prettyExp (SUBEXP se) = se
  | prettyExp (BINOP (f, (x, y))) =
      f ^ "(" ^ x ^ ", " ^ y ^ ")"
  | prettyExp (APPLY (f, args, types)) =
      "apply " ^ f ^ "(" ^ punctuate ", " (map prettySubExp args) ^ ")" ^ " : "
      ^ "{" ^ punctuate ", " (map prettyType types) ^ "}"
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

fun prettyFunDef (FUNDEF {name, params, rettype, body}) =
  "fun " ^ name ^ "(" ^ (punctuate ", " (map prettyParam params)) ^ ")" ^ "\n"
  ^ ": " ^ "{" ^ punctuate ", " (map prettyRetType rettype) ^ "}" ^ " = "
  ^ prettyBody body

fun prettyProg (PROG ((), stms, funs)) =
  punctuate "\n" (map prettyStm stms @ map prettyFunDef funs)
