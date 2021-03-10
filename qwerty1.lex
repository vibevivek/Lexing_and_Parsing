structure Tokens= Tokens
  
  type pos = int
  type svalue = Tokens.svalue
  type ('a,'b) token = ('a,'b) Tokens.token  
  type lexresult = (svalue, pos) token

  val pos = ref 0
  val eof = fn () => Tokens.EOF(!pos, !pos)

  val error = fn (e, e1, l:int, _) => TextIO.output(TextIO.stdOut,e ^ (Int.toString l) ^ ":" ^ e1 ^ "\n")
  

  fun revfold _ nil b = b
  | revfold f (hd::tl) b = revfold f tl (f(hd,b))
  
%%
%header (functor CalcLexFun(structure Tokens:Calc_TOKENS));
alpha=[A-Za-z];
ws = [\ \t\n];
%%

{ws}+    => (lex());
"(" => (Tokens.LPAREN(!pos,!pos));
")" => (Tokens.RPAREN(!pos,!pos));
";" => (Tokens.TERM(!pos,!pos));
{alpha}+ => (if (yytext = "TRUE" orelse yytext = "FALSE") then Tokens.CONST(yytext,!pos,!pos)
            else
              if (yytext = "OR") then Tokens.OR(!pos,!pos)
              else
                if (yytext = "AND") then Tokens.AND(!pos,!pos)
                else
                  if (yytext = "XOR") then Tokens.XOR(!pos,!pos)
                  else
                    if (yytext = "EQUALS") then Tokens.EQUALS(!pos,!pos)
                    else
                      if (yytext = "IMPLIES") then Tokens.IMPLIES(!pos,!pos)
                      else
                        if (yytext = "IF") then Tokens.IF(!pos,!pos)
                        else
                          if (yytext = "THEN") then Tokens.THEN(!pos,!pos)
                          else
                            if (yytext = "ELSE") then Tokens.ELSE(!pos,!pos)
                            else 
                              if (yytext = "NOT") then Tokens.NOT(!pos,!pos)
                              else Tokens.ID(yytext,!pos,!pos)

          );

.      => (error ("Unknown Token: ",yytext,!pos,!pos);
             lex());



