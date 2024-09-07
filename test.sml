fun main () =
  let
    val srcname = List.nth (CommandLine.arguments (), 0)
    val s = TextIO.inputAll (TextIO.openIn srcname)
  in
    (*
        case tokeniseCoreFuthark {srcname = srcname, input = s} of
          NO errmsg => print errmsg
        | OK tokens =>
            List.app (fn (t, _) => print (Token.pp_token t ^ "\n")) tokens
    *)
    case parseCoreFuthark {srcname = srcname, input = s} of
      NO errmsg => print errmsg
    | OK prog => print (prettyProg prog ^ "\n")
  end


val () = main ()
