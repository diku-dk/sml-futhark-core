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
    | pp_token (STRING s) = "\"" ^ s ^ "\""
    | pp_token (WORD s) = s
end

fun repeat 0 _ x = x
  | repeat n f x =
      repeat (n - 1) f (f x)

fun constituent #"_" = true
  | constituent #"*" = true
  | constituent #"+" = true
  | constituent #"/" = true
  | constituent #"-" = true
  | constituent #"." = true
  | constituent c =
      Char.isAlphaNum c orelse ord c >= 128

fun nextToken (loc: Region.loc) (s: Substring.substring) =
  let
    fun charTok t s' =
      SOME ((t, Region.mkReg (loc, loc)), Region.next loc, s')
  in
    case Substring.getc s of
      NONE => NONE
    | SOME (#"(", s') => charTok Token.LPAREN s'
    | SOME (#")", s') => charTok Token.RPAREN s'
    | SOME (#"{", s') => charTok Token.LBRACE s'
    | SOME (#"}", s') => charTok Token.RBRACE s'
    | SOME (#"[", s') => charTok Token.LBRACKET s'
    | SOME (#"]", s') => charTok Token.RBRACKET s'
    | SOME (#"<", s') => charTok Token.LANGLE s'
    | SOME (#">", s') => charTok Token.RANGLE s'
    | SOME (#":", s') => charTok Token.COLON s'
    | SOME (#"=", s') => charTok Token.EQ s'
    | SOME (#",", s') => charTok Token.COMMA s'
    | SOME (#"?", s') => charTok Token.QUESTION s'
    | SOME (#"@", s') => charTok Token.AT s'
    | SOME (#"\\", s') => charTok Token.BACKSLASH s'
    | SOME (#"*", s') => charTok Token.ASTERISK s'
    | SOME (#"-", s') =>
        (case Substring.getc s' of
           SOME (#">", s') =>
             let
               val loc' = Region.next loc
             in
               SOME
                 ( (Token.ARROW, Region.mkReg (loc, loc'))
                 , (Region.next loc')
                 , s'
                 )
             end
         | _ => charTok Token.DASH s')
    | SOME (#"#", s') => charTok Token.HASH s'
    | SOME (#"\n", s') => nextToken (Region.newline loc) s'
    | SOME (#" ", s') => nextToken (Region.next loc) s'
    | SOME (#"\"", s') =>
        let
          val (lit, s') = Substring.splitl (fn c => c <> #"\"") s'
          val loc' = repeat (Substring.size lit + 1) Region.next loc
        in
          case Substring.getc s' of
            SOME (#"\"", s') =>
              SOME
                ( ( Token.STRING (Substring.string lit)
                  , Region.mkReg (loc, loc')
                  )
                , (Region.next loc')
                , s'
                )
          | _ =>
              raise Fail
                ("Lexical error at " ^ Region.ppLoc loc
                 ^ "\nUnclosed string literal.")
        end
    | SOME (c, _) =>
        if constituent c then
          let
            val (w, s') = Substring.splitl constituent s
            val loc' = repeat (Substring.size w - 1) Region.next loc
          in
            SOME
              ( (Token.WORD (Substring.string w), Region.mkReg (loc, loc'))
              , Region.next loc'
              , s'
              )
          end
        else
          raise Fail
            ("Lexical error at " ^ Region.ppLoc loc ^ "\nCharacter: " ^ str c
             ^ " (" ^ Int.toString (ord c) ^ ")\n")
  end

fun tokenise srcname s =
  let
    fun loop loc s =
      case nextToken loc s of
        NONE => []
      | SOME (tok, loc', s') => tok :: loop loc' s'
  in
    loop (Region.loc0 srcname) (Substring.full s)
  end

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

  val pBinOp = eat (WORD "add32") *> accept "add32"

  val pRetAls: RetAls p =
    parens
      (liftM2 RETALS (brackets (sepBy pInt (eat COMMA)) <* eat COMMA, brackets
         (sepBy pInt (eat COMMA))))

  val pRet = pRetType >>> choice [eat HASH *> pRetAls, accept (RETALS ([], []))]

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
      fun mk fname params ret body =
        (FUNDEF {name = fname, params = params, ret = ret, body = body})
    in
      mk <$> (eat (WORD "fun") *> pWord) <*> parens (sepBy pParam (eat COMMA))
      <*> (eat COLON *> braces (sepBy pRet (eat COMMA)))
      <*> (eat EQ *> braces (delay0 pBody))
    end

in
  val pProg: Prog FutParse.p =
    liftM3 PROG (pTypes, many (pStm ()), many pFunDef) <* eof
end
