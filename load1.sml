structure CalcLrVals = CalcLrValsFun(structure Token = LrParser.Token)
structure CalcLex = CalcLexFun(structure Tokens = CalcLrVals.Tokens);
structure CalcParser =
      Join(structure LrParser = LrParser
               structure ParserData = CalcLrVals.ParserData
               structure Lex = CalcLex)
     
fun invoke lexstream =
                let fun print_error (s,pos:int,_) =
                TextIO.output(TextIO.stdOut, "Syntax Error:" ^ (Int.toString pos) ^ ":" ^ s ^ "\n")
        in
            CalcParser.parse(0,lexstream,print_error,())
        end
(*=========*)
fun readlist1(infile)=
    let
        val ins = TextIO.openIn infile
        val result = TextIO.inputAll ins

    in
        result
    end


fun stringToLexer(str) =
    let val done = ref false
        val lexer=  CalcParser.makeLexer (fn _ => if (!done) then "" else (done:=true;str))
    in
    lexer
    end

fun quit1(outFile: string, list: string list) =
  let
    val outStream = TextIO.openOut outFile
    fun out(xs : string list) =  
          case xs of
              [] => (TextIO.closeOut outStream)
            | x::xs' => if (xs' = []) then (TextIO.output(outStream, x); out(xs'))
                        else  (TextIO.output(outStream, x ^ ", "); out(xs'))
  in
    out(list)
  end;




fun parseTolex (xs : string list) =
    let
        val lexx = []
        fun intern (xs: string list, ys: string list)=
            if (null xs) then ys
            else
                        if (hd(xs) = "FORMULA") then intern(tl(xs), ys)
                            else
                                if (hd(xs) = "STATEMENT") then intern(tl(xs), ys)
                                else
                                    if (hd(xs) = "PROGRAM") then intern(tl(xs), ys)
                                    else hd(xs)::intern(tl(xs),ys)
    in 
        intern(xs, lexx)
    end

fun distroy (s : string) = 
    String.tokens Char.isDigit s;

fun distroy1 (s : string) =
    quit1("output_file.txt", distroy(s))

fun distroy2 (s : string)=
    parseTolex(distroy(s))

fun merger(xs: string list, ys: string list)=
    xs @ ["9"] @ ys

fun exchange(xs: string list)=
    let
        val final = []
        fun intern1 (xs: string list)=
            if (null xs) then final
            else
                if (hd(xs) = "9") then "\n"::intern1(tl(xs))
                else
                    hd(xs)::intern1(tl(xs))
    in
        intern1(xs)
    end




(*==========*)
(*fun stringToLexer str =
    let val done = ref false
        val lexer=  CalcParser.makeLexer (fn _ => if (!done) then "" else (done:=true;str))
    in
    lexer
    end 
    *)


(*)
fun distroy (s : string) = 
    String.tokens Char.isDigit s;
    *)
        
fun parse (lexer) =
    let val dummyEOF = CalcLrVals.Tokens.EOF(0,0)
        val (result, lexer) = invoke lexer
    val (nextToken, lexer) = CalcParser.Stream.get lexer
    in
        if CalcParser.sameToken(nextToken, dummyEOF) then quit1("output_file1.txt",exchange(merger(distroy2(result), distroy(result))))

    else (TextIO.output(TextIO.stdOut, "Warning: Unconsumed input \n"); quit1("output_file1.txt",exchange(merger(distroy2(result), distroy(result)))))
    end





val parseString = parse o stringToLexer



(*val parseInputFile = parse o stringToLexer*)


