structure Token =
struct
  datatype token =
    LBRACE
  | RBRACE
  | LPAREN
  | RPAREN
  | LBRACKET
  | RBRACKET
  | LANGLE
  | RANGLE
  | COLON
  | EQ
  | COMMA
  | BACKSLASH
  | ASTERISK
  | DASH
  | ARROW
  | HASH
  | QUESTION
  | AT
  | COLONPLUS
  | STRING of string
  | WORD of string
  fun pp_token LBRACE = "{"
    | pp_token RBRACE = "}"
    | pp_token LPAREN = "("
    | pp_token RPAREN = ")"
    | pp_token LBRACKET = "["
    | pp_token RBRACKET = "]"
    | pp_token LANGLE = "<"
    | pp_token RANGLE = ">"
    | pp_token COLON = ":"
    | pp_token EQ = "="
    | pp_token COMMA = ","
    | pp_token QUESTION = "?"
    | pp_token AT = "@"
    | pp_token BACKSLASH = "\\"
    | pp_token ASTERISK = "*"
    | pp_token DASH = "-"
    | pp_token ARROW = "->"
    | pp_token HASH = "#"
    | pp_token COLONPLUS = ":+"
    | pp_token (STRING s) = "\"" ^ s ^ "\""
    | pp_token (WORD s) = s
end
local
  structure CharParser = Parse(CharToken)

  local
    open CharParser
    open Token
    infix >>= <|> <*> <* *> <$> <$$> >>> ?? ??? ??*

    fun manyTill p pend =
      let
        fun loop acc =
          choice
            [ pend >>= (fn y => accept (rev acc, y))
            , p >>= (fn x => loop (x :: acc))
            ]
      in
        loop []
      end

    fun tok c t = eat c *> accept t

    fun constituent #"_" = true
      | constituent #"*" = true
      | constituent #"+" = true
      | constituent #"/" = true
      | constituent #"." = true
      | constituent c =
          Char.isAlphaNum c orelse ord c >= 128

    val space = satisfy Char.isSpace

    val pKeyword =
      implode <$> some (satisfy (fn c => Char.isAlphaNum c orelse c = #"_"))

    val pTag = eat #"_" *> some (satisfy Char.isDigit)

    fun mapM_ _ [] = accept ()
      | mapM_ f (x :: xs) =
          f x >>= (fn _ => mapM_ f xs)

    fun string s =
      mapM_ eat (explode s) *> accept s

    val pExprBox =
      (fn (s, _) => "<{" ^ implode s ^ "}>")
      <$> (string "<{" *> manyTill next (string "}>"))

    val pName =
      (fn (x, tag) => concat x ^ "_" ^ implode tag)
      <$> manyTill (choice [pExprBox, str <$> satisfy constituent]) pTag

    val pNumNoSign = implode <$> some (satisfy Char.isDigit)

    val pNum = choice
      [eat #"-" *> ((fn s => "-" ^ s) <$> pNumNoSign), pNumNoSign]

    val pDec =
      pNum
      >>=
      (fn x =>
         choice
           [(eat #"." *> pNum) >>= (fn y => accept (x ^ "." ^ y)), accept x])

    val pConstant =
      (fn (x, y) => x ^ y)
      <$>
      (choice
         [(pDec
           >>=
           (fn x =>
              choice [(eat #"e" *> pNum >>= (fn y => accept (x ^ y))), accept x]))]
       >>> pKeyword)

    val pWord: string p = choice [pName, pConstant, pKeyword]

    val pStringLit =
      eat #"\"" *> many (satisfy (fn c => c <> #"\"")) <* eat #"\""

    fun id x = x

    val pToken: (token * Region.reg) p =
      (id
       <$$>
       choice
         ([WORD <$> pWord, STRING o implode <$> pStringLit]
          @
          [ tok #"{" LBRACE
          , tok #"}" RBRACE
          , tok #"(" LPAREN
          , tok #")" RPAREN
          , tok #"[" LBRACKET
          , tok #"]" RBRACKET
          , tok #"<" LANGLE
          , tok #">" RANGLE
          , string ":+" *> accept COLONPLUS
          , tok #":" COLON
          , tok #"=" EQ
          , tok #"," COMMA
          , tok #"?" QUESTION
          , tok #"@" AT
          , tok #"\\" BACKSLASH
          , tok #"*" ASTERISK
          , tok #"-" DASH
          , tok #"#" HASH
          , string "->" *> accept ARROW
          ])) <* many space
  in
    fun tokenise input =
      CharParser.parse (many pToken <* eof) (CharToken.tokenise input)
  end
in
  structure FutParse = Parse(Token)

  local
    open FutParse
    open Token
    infix >>= <|> <*> <* *> <$> <$$> >>> ?? ??? ??*

    fun liftM2 f (x, y) =
      (fn x' => fn y' => f (x', y')) <$> x <*> y
    fun liftM3 f (x, y, z) =
      (fn x' => fn y' => fn z' => f (x', y', z')) <$> x <*> y <*> z

    fun braces p =
      eat LBRACE *> p <* eat RBRACE
    fun parens p =
      eat LPAREN *> p <* eat RPAREN
    fun brackets p =
      eat LBRACKET *> p <* eat RBRACKET

    fun sepBy1 p sep =
      p
      >>=
      (fn x =>
         choice
           [ sep *> delay (sepBy1 p) sep >>= (fn xs => accept (x :: xs))
           , delay accept [x]
           ])

    fun sepBy p sep =
      choice [delay (sepBy1 p) sep, delay accept []]

    fun delay0 p = delay p ()

    val pWord =
      next
      >>=
      (fn t =>
         case t of
           WORD w => accept w
         | _ => reject "expected identifier")

    val pInt =
      pWord
      >>=
      (fn w =>
         case Int.fromString w of
           SOME x => accept x
         | NONE => reject "expected integer")

    fun pAttr () =
      choice
        [ ATTR_INT <$> pInt
        , liftM2 ATTR_COMP (pWord, parens (sepBy (delay0 pAttr) (eat COMMA)))
        , ATTR_NAME <$> pWord
        ]

    val pAttrs = many (eat HASH *> brackets (delay0 pAttr))

    val pTypes = eat (WORD "types") *> eat LBRACE *> eat RBRACE

    val pSubExp = choice [eat LPAREN *> eat RPAREN *> accept "()", pWord]

    val pIntType = choice
      [ eat (WORD "i8") *> accept I8
      , eat (WORD "i16") *> accept I16
      , eat (WORD "i32") *> accept I32
      , eat (WORD "i64") *> accept I64
      ]

    val pFloatType = choice
      [ eat (WORD "f16") *> accept F16
      , eat (WORD "f32") *> accept F32
      , eat (WORD "f64") *> accept F64
      ]

    fun pType' () =
      choice
        [ eat (WORD "unit") *> accept UNIT
        , eat (WORD "bool") *> accept BOOL
        , INT <$> pIntType
        , FLOAT <$> pFloatType
        , liftM2 ARRAY (brackets pSubExp, delay0 pType')
        ]

    val pType = pType' ()

    val pPatElem = pWord >>> (eat COLON *> pType)

    val pPat: Pat p = PAT <$> braces (sepBy pPatElem (eat COMMA))

    val pParam = pWord >>> (eat COLON *> pType)

    val pRetType = choice
      [UNIQUE <$> (eat ASTERISK *> pType), NONUNIQUE <$> pType]

    val pUnOp =
      let
        fun opt s =
          eat (WORD s) *> accept s
      in
        (choice o map opt)
          ["ssignum8", "ssignum16", "ssignum32", "ssignum64", "not"]
      end

    val pBinOp =
      let
        fun opt s =
          eat (WORD s) *> accept s
      in
        (choice o map opt)
          [ (* integer arithmetic *) "add8"
          , "add16"
          , "add32"
          , "add64"
          , "mul8"
          , "mul16"
          , "mul32"
          , "mul64"
          , "sub8"
          , "sub16"
          , "sub32"
          , "sub64"
          (* float arithmetic *)
          , "fadd16"
          , "fadd32"
          , "fadd64"
          , "fmul16"
          , "fmul32"
          , "fmul64"
          , "fdiv16"
          , "fdiv32"
          , "fdiv64"
          , "fsub16"
          , "fsub32"
          , "fsub64"
          (* logical operations *)
          , "logor"
          , "logand"
          (* comparisons *)
          , "eq_i8"
          , "eq_i16"
          , "eq_i32"
          , "eq_i64"
          , "eq_f16"
          , "eq_f32"
          , "eq_f64"
          , "eq_bool"
          , "sle8"
          , "sle16"
          , "sle32"
          , "sle64"
          , "slt8"
          , "slt16"
          , "slt32"
          , "slt64"
          ]
      end


    val pRetAls: RetAls p =
      parens
        (liftM2 RETALS (brackets (sepBy pInt (eat COMMA)) <* eat COMMA, brackets
           (sepBy pInt (eat COMMA))))

    val pRet =
      pRetType >>> choice [eat HASH *> pRetAls, accept (RETALS ([], []))]

    fun pConvOp s cop t1 t2 =
      let
        fun cop' f se t =
          (cop (f, t), se)
      in
        eat (WORD s) *> (cop' <$> t1 <*> pSubExp <*> (eat (WORD "to") *> t2))
      end

    fun pExp () =
      choice
        [ eat (WORD "apply")
          *>
          liftM3 APPLY
            ( pWord
            , parens (sepBy pSubExp (eat COMMA))
            , eat COLON *> braces (sepBy pRet (eat COMMA))
            )
        , SOAC <$> delay0 pSOAC
        , CONVOP <$> pConvOp "sext" SEXT pIntType pIntType
        , liftM2 BINOP (pBinOp, parens (pSubExp >>> (eat COMMA *> pSubExp)))
        , liftM2 UNOP (pUnOp, pSubExp)
        , SUBEXP <$> pSubExp
        ]

    and pStm () =
      liftM2 STM (eat (WORD "let") *> pPat, eat EQ *> delay0 pExp)

    and pBody () : Body p =
      choice
        [ BODY <$> (accept [] >>> (braces (sepBy pSubExp (eat COMMA))))
        , BODY
          <$>
          (many (delay0 pStm)
           >>> (eat (WORD "in") *> braces (sepBy pSubExp (eat COMMA))))
        ]

    and pSOAC () =
      choice
        [eat (WORD "map")
         *>
         parens (liftM3 MAP
           ( pSubExp <* eat COMMA
           , braces (sepBy pWord (eat COMMA)) <* eat COMMA
           , delay0 pLambda
           ))]

    and pLambda () =
      eat BACKSLASH
      *>
      liftM3 LAMBDA
        ( braces (many pParam)
        , eat COLON *> braces (sepBy pType (eat COMMA))
        , eat ARROW *> delay0 pBody
        )

    val pFunDef =
      let
        fun mk attrs fname params ret body =
          (FUNDEF
             { attrs = attrs
             , name = fname
             , params = params
             , ret = ret
             , body = body
             })
      in
        mk <$> pAttrs <*> (eat (WORD "fun") *> pWord)
        <*> parens (sepBy pParam (eat COMMA))
        <*> (eat COLON *> braces (sepBy pRet (eat COMMA)))
        <*> (eat EQ *> braces (delay0 pBody))
      end


    val pProg: Prog FutParse.p =
      liftM3 PROG (pTypes, many (pStm ()), many pFunDef) <* eof

  in
    datatype 'a result = NO of string | OK of 'a

    fun tokeniseCoreFuthark input : (token * Region.reg) list result =
      case tokenise input of
        CharParser.NO (loc, errmsg) =>
          NO ("Lexical error.\n" ^ Region.ppLoc loc ^ ": " ^ errmsg () ^ "\n")
      | CharParser.OK tokens => OK tokens

    fun parseCoreFuthark input =
      case tokenise input of
        CharParser.NO (loc, errmsg) =>
          NO ("Lexical error.\n" ^ Region.ppLoc loc ^ ": " ^ errmsg () ^ "\n")
      | CharParser.OK tokens =>
          case FutParse.parse pProg tokens of
            FutParse.NO (loc, errmsg) =>
              NO
                ("Syntax error.\n" ^ Region.ppLoc loc ^ ": " ^ errmsg () ^ "\n")
          | FutParse.OK prog => OK prog

  end
end
