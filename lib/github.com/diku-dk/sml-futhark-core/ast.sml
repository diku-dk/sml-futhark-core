type SubExp = string

datatype IntType = I8 | I16 | I32 | I64

datatype FloatType = F16 | F32 | F64

datatype Attr =
  ATTR_NAME of string
| ATTR_INT of int
| ATTR_COMP of string * Attr list

type Attrs = Attr list

datatype Type =
  UNIT
| BOOL
| INT of IntType
| FLOAT of FloatType
| ARRAY of SubExp * Type

datatype RetType = UNIQUE of Type | NONUNIQUE of Type

datatype Pat = PAT of (string * Type) list

type Param = string * Type

type Result = string list

datatype RetAls = RETALS of int list * int list

datatype ConvOp = SEXT of IntType * IntType

datatype Exp =
  SUBEXP of SubExp
| BINOP of string * (SubExp * SubExp)
| CONVOP of ConvOp * SubExp
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
    { attrs: Attrs
    , name: string
    , params: Param list
    , ret: (RetType * RetAls) list
    , body: Body
    }

datatype Prog = PROG of unit * Stm list * Fundef list
