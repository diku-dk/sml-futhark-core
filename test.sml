fun main () =
  let
    val srcname = List.nth (CommandLine.arguments (), 0)
    val s = TextIO.inputAll (TextIO.openIn srcname)
    val tokens = tokenise srcname s
  in
    case FutParse.parse pProg tokens of
      FutParse.NO (loc, errmsg) =>
        print (Region.ppLoc loc ^ ": " ^ errmsg () ^ "\n")
    | FutParse.OK prog => print (prettyProg prog)
  end


val () = main ()
