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

datatype RetAls = RETALS of int list * int list

datatype Exp =
  SUBEXP of SubExp
| BINOP of string * (SubExp * SubExp)
| APPLY of string * SubExp list * (RetType * RetAls) list
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
    {name: string, params: Param list, ret: (RetType * RetAls) list, body: Body}

datatype Prog = PROG of unit * Stm list * Fundef list
