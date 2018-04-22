type pos = int
type lexresult = Tokens.token

val lineNum = ErrorMsg.lineNum
val linePos = ErrorMsg.linePos
fun err(p1,p2) = ErrorMsg.error p1

val nestedCommentStack = ref 0 ;  
val strings = ref "" ; 
val stringsPos = ref 0; 
val stringsBal = ref true;

fun convertToString(str1) = 
    let
      val SOME ddd = Int.fromString str1
    in
      String.str (Char.chr ddd)
    end 

fun eof() =
    let val pos = hd(!linePos)
    in
        if (!nestedCommentStack <> 0) then (ErrorMsg.error pos ("unbalanced comments"))
        else if (!stringsBal = false) then (ErrorMsg.error pos ("unclosed strings"))
        else () ;
        Tokens.EOF(pos,pos)
    end

%%

ids = [a-zA-Z][0-9a-zA-Z_]* ;
digits = [0-9]+ ;
chars = ( (0[0-9][0-9]) | (1[0-1][0-9]) | (12[1-7]) )+ ;
newlines = [\n\r]+ ;
controls = \^[_@A-Z[\].^] ;
spaces = [\t\f]+ ;

%s COMMENT STRING ESCAPE;

%%

<INITIAL>" "                                 => (continue());
<INITIAL>"type"                     => (Tokens.TYPE(yypos,yypos+4));
<INITIAL>"var"                      => (Tokens.VAR(yypos,yypos+3));
<INITIAL>"function"                 => (Tokens.FUNCTION(yypos,yypos+8));
<INITIAL>"break"                    => (Tokens.BREAK(yypos,yypos+5));
<INITIAL>"of"                       => (Tokens.OF(yypos,yypos+2));
<INITIAL>"end"                      => (Tokens.END(yypos,yypos+3));
<INITIAL>"in"                       => (Tokens.IN(yypos,yypos+2));
<INITIAL>"nil"                      => (Tokens.NIL(yypos,yypos+3));
<INITIAL>"let"                      => (Tokens.LET(yypos,yypos+3));
<INITIAL>"do"                       => (Tokens.DO(yypos,yypos+2));
<INITIAL>"to"                       => (Tokens.TO(yypos,yypos+2));
<INITIAL>"for"                      => (Tokens.FOR(yypos,yypos+3));
<INITIAL>"while"                    => (Tokens.WHILE(yypos,yypos+5));
<INITIAL>"else"                     => (Tokens.ELSE(yypos,yypos+4));
<INITIAL>"then"                     => (Tokens.THEN(yypos,yypos+4));
<INITIAL>"if"                       => (Tokens.IF(yypos,yypos+2));
<INITIAL>"array"                    => (Tokens.ARRAY(yypos,yypos+5));

<INITIAL>":="                       => (Tokens.ASSIGN(yypos,yypos+2));
<INITIAL>"|"                        => (Tokens.OR(yypos,yypos+1));
<INITIAL>"&"                        => (Tokens.AND(yypos,yypos+1));
<INITIAL>">="                       => (Tokens.GE(yypos,yypos+2));
<INITIAL>">"                        => (Tokens.GT(yypos,yypos+1));
<INITIAL>"<="                       => (Tokens.LE(yypos,yypos+2));
<INITIAL>"<"                        => (Tokens.LT(yypos,yypos+1));
<INITIAL>"<>"                       => (Tokens.NEQ(yypos,yypos+2));
<INITIAL>"="                        => (Tokens.EQ(yypos,yypos+1));
<INITIAL>"/"                        => (Tokens.DIVIDE(yypos,yypos+1));
<INITIAL>"*"                        => (Tokens.TIMES(yypos,yypos+1));
<INITIAL>"-"                        => (Tokens.MINUS(yypos,yypos+1));
<INITIAL>"+"                        => (Tokens.PLUS(yypos,yypos+1));
<INITIAL>"."                        => (Tokens.DOT(yypos,yypos+1));
<INITIAL>"}"                        => (Tokens.RBRACE(yypos,yypos+1));
<INITIAL>"{"                        => (Tokens.LBRACE(yypos,yypos+1));
<INITIAL>"]"                        => (Tokens.RBRACK(yypos,yypos+1));
<INITIAL>"["                        => (Tokens.LBRACK(yypos,yypos+1));
<INITIAL>")"                        => (Tokens.RPAREN(yypos,yypos+1));
<INITIAL>"("                        => (Tokens.LPAREN(yypos,yypos+1));
<INITIAL>";"                        => (Tokens.SEMICOLON(yypos,yypos+1));
<INITIAL>":"                        => (Tokens.COLON(yypos,yypos+1));
<INITIAL>","                        => (Tokens.COMMA(yypos,yypos+1));

<INITIAL>"/*"                       => ( nestedCommentStack := !nestedCommentStack+1 ; YYBEGIN COMMENT ; continue() );
<INITIAL>"\""                       => ( stringsBal := false ; strings := "" ; stringsPos := yypos ; YYBEGIN STRING ; continue() );
<INITIAL>{ids}                      => ( Tokens.ID(yytext,yypos,yypos+(size yytext)) );
<INITIAL>{digits}                   => ( Tokens.INT(valOf(Int.fromString yytext),yypos,yypos+(size yytext)) );
<INITIAL>{spaces}                   => ( linePos := yypos :: !linePos; continue() );
<INITIAL>{newlines}                 => ( lineNum := !lineNum+1; linePos := yypos :: !linePos; continue() );
  
<COMMENT>"/*"                       => ( nestedCommentStack := !nestedCommentStack+1 ; continue() );
<COMMENT>"*/"                       => ( nestedCommentStack := !nestedCommentStack-1; if (!nestedCommentStack = 0) then YYBEGIN INITIAL else (); continue() );
<COMMENT>{newlines}                 => ( lineNum := !lineNum+1; linePos := yypos :: !linePos; continue() );
<COMMENT>{spaces}                   => ( linePos := yypos :: !linePos; continue() );
<COMMENT>.                          => ( continue() );


<STRING>"\""                        => ( stringsBal := true ; YYBEGIN INITIAL ; Tokens.STRING(!strings,!stringsPos,yypos) );
<STRING>"\\"                        => ( YYBEGIN ESCAPE ; continue() );
<STRING>{newlines}                  => ( lineNum := !lineNum+1 ; linePos := yypos :: !linePos; continue() );
<STRING>.                           => ( strings := !strings^yytext ; continue() );

<ESCAPE>{newlines}                  => ( lineNum := !lineNum+1 ; linePos := yypos :: !linePos ; continue() );
<ESCAPE>{chars}                     => ( strings := !strings^(convertToString(yytext)) ; YYBEGIN STRING ; continue() );
<ESCAPE>"\""                        => ( strings := !strings^"\"" ; YYBEGIN STRING ; continue() );
<ESCAPE>"t"                         => ( strings := !strings^"\t" ; YYBEGIN STRING ; continue() );
<ESCAPE>"n"                         => ( strings := !strings^"\n" ; YYBEGIN STRING ; continue() );
<ESCAPE>"f"                         => ( strings := !strings^"\f" ; YYBEGIN STRING ; continue() );
<ESCAPE>"r"                         => ( strings := !strings^"\r" ; YYBEGIN STRING ; continue() );
<ESCAPE>"\\"                        => ( strings := !strings^"\\" ; YYBEGIN STRING ; continue() );
<ESCAPE>{controls}                  => ( strings := !strings^yytext ; YYBEGIN STRING ; continue() );
<ESCAPE>({newlines}|{spaces})"\\"   => ( YYBEGIN STRING ; continue() );


.                                   => ( ErrorMsg.error yypos ("illegal character " ^ yytext) ; continue() );
