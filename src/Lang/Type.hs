module Lang.Type where

data Tag = ERR
         | END
         | ID
         | KW_INT | KW_CHAR |KW_VOID
         | KW_EXTERN
         | NUM | CH | STR
         | NOT | LEA
         | ADD | SUB | MUL | DIV | MOD
         | INC | DEC
         | GT | GE | LT | LE | EQU |NEQU
         | AND | OR
         | LPAREN | RPAREN
         | LBRACK | RBRACK
         | COMMA | COLON | SEMICOLON
         | ASSIGN
         | KW_IF | KW_ELSE
         | KW_SWITCH | KW_CASE | KW_DEFAULT
         | KW_WHILE | KW_DO | KW_FOR
         | KW_BREAK | KW_CONTINUE | KW_RETURN
         deriving(Show)

data Token = Char Char
           | Id String
           | Num String
           | Str String
           | Token String
           deriving (Show)

keyword :: String -> Tag
keyword "int" = KW_INT
keyword "char" = KW_CHAR
keyword "void" = KW_VOID
keyword "extern" = KW_EXTERN
keyword "if" = KW_IF
keyword "else" = KW_ELSE
keyword "switch" = KW_SWITCH
keyword "case" = KW_CASE
keyword "default" = KW_DEFAULT
keyword "while" = KW_WHILE
keyword "do" = KW_DO
keyword "for" = KW_FOR
keyword "break" = KW_BREAK
keyword "continue" = KW_CONTINUE
keyword "return" = KW_RETURN
keyword _ = ID