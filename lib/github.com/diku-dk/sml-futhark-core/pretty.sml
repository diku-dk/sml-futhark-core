local
  open Pretty
  infix ^+^ ^^

  fun x ^+^ y =
    x ^^ text " " ^^ y

  fun parens x =
    text "(" ^^ x ^^ text ")"

  fun braces x =
    text "{" ^^ x ^^ text "}"

  fun brackets x =
    text "[" ^^ x ^^ text "]"

  fun intersperse y [] = []
    | intersperse y [x] = [x]
    | intersperse y (x :: xs) =
        x :: y :: intersperse y xs

  fun punctuate c = concat o intersperse c

  fun prettySubExp se = text se

  fun prettyAttr (ATTR_INT x) = int x
    | prettyAttr (ATTR_NAME x) = text x
    | prettyAttr (ATTR_COMP (x, attrs)) =
        text x ^^ parens (punctuate (text ", ") (map prettyAttr attrs))

  val prettyAttrs =
    punctuate newline o map (fn x => text "#" ^^ brackets (prettyAttr x))

  fun prettyIntType I8 = text "i8"
    | prettyIntType I16 = text "i16"
    | prettyIntType I32 = text "i32"
    | prettyIntType I64 = text "i64"

  fun prettyFloatType F16 = text "f16"
    | prettyFloatType F32 = text "f32"
    | prettyFloatType F64 = text "f64"

  fun prettyType UNIT = text "unit"
    | prettyType BOOL = text "bool"
    | prettyType (INT x) = prettyIntType x
    | prettyType (FLOAT x) = prettyFloatType x
    | prettyType (ARRAY (d, t)) =
        brackets (prettySubExp d) ^^ prettyType t

  fun prettyRetType (UNIQUE t) = text "*" ^^ prettyType t
    | prettyRetType (NONUNIQUE t) = prettyType t

  fun prettyRetAls (RETALS (pals, rals)) =
    parens (punctuate (text ", ")
      [ brackets (punctuate (text ",") (map int pals))
      , brackets (punctuate (text ",") (map int rals))
      ])

  fun prettyRet ret =
    braces (punctuate (text ", ")
      (map (fn (t, als) => prettyRetType t ^^ text "#" ^^ prettyRetAls als) ret))

  fun prettyParam (v, t) =
    text v ^^ text ":" ^+^ prettyType t

  fun prettyPat (PAT pes) =
    braces (punctuate (text ", ") (map prettyParam pes))

  fun prettyExp (SUBEXP se) = prettySubExp se
    | prettyExp (BINOP (f, (x, y))) =
        text f ^^ parens (prettySubExp x ^^ text ", " ^^ prettySubExp y)
    | prettyExp (APPLY (f, args, ret)) =
        text "apply" ^+^ text f
        ^^ parens (punctuate (text ", ") (map prettySubExp args)) ^+^ text ":"
        ^+^ prettyRet ret
    | prettyExp (CONVOP (SEXT (ft, tt), se)) =
        punctuate (text " ")
          [text "sext", prettyIntType ft, prettySubExp se, prettyIntType tt]
    | prettyExp (SOAC soac) = prettySoac soac

  and prettyStm (STM (pat, exp)) =
    (nest 2 o group)
      (text "let" ^+^ prettyPat pat ^+^ text "=" ^^ newline ^^ prettyExp exp)

  and prettyBody (BODY (stms, res)) =
    (braces o nest 2 o group)
      (newline
       ^^
       (punctuate newline
          (map prettyStm stms
           @
           [text "in {" ^^ punctuate (text ", ") (map prettySubExp res)
            ^^ text "}"])))

  and prettySoac (MAP (w, arrs, lam)) =
    text "map"
    ^^
    parens
      (prettySubExp w ^^ text ", "
       ^^ braces (punctuate (text ",") (map text arrs)) ^^ text ","
       ^+^ prettyLambda lam)

  and prettyLambda (LAMBDA (params, types, body)) =
    text "\\ {" ^^ punctuate (text " ") (map prettyParam params) ^^ text "} : {"
    ^^ punctuate (text ", ") (map prettyType types) ^^ text "} -> "
    ^^ prettyBody body

  fun prettyFunDef (FUNDEF {attrs, name, params, ret, body}) =
    prettyAttrs attrs ^^ text "fun" ^+^ text name
    ^^ parens ((punctuate (text ", ") (map prettyParam params))) ^^ newline
    ^^ text ": " ^^ prettyRet ret ^^ text " = " ^^ prettyBody body

in
  fun prettyProg (PROG ((), stms, funs)) =
    toString 100
      (concat (map prettyStm stms) ^^ newline ^^ concat (map prettyFunDef funs))
end
