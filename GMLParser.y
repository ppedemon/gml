{
module GMLParser(parse) where 

import Array(listArray)

import GMLSyn
import GMLLexer
}

%name      parse
%tokentype { GMLTok } 

%token 
INT    { TInt    _ $$ }
REAL   { TReal   _ $$ }
BOOL   { TBool   _ $$ }
STRING { TLit    _ $$ }
BINDER { TBinder _ $$ }
IDENT  { TId _ $$     }
OP     { TOp _ $$     }
'{'    { TLCurly _    }
'}'    { TRCurly _    }
'['    { TLBracket _  }
']'    { TRBracket _  }
EOF    { EOF          } 
%%

TokenGroup :: { Either Prog String }
 : TokenList EOF        { Left (reverse $1) }

TokenList :: { Prog }
 : {- Empty -}          { []      }
 | TokenList Token      { $2 : $1 }
 
Token :: { Token }
 : '{' TokenList '}'    { Function (reverse $2) }
 | '[' TokenList ']'    { Arr (listArray (0,length $2 - 1) (reverse $2)) }
 | INT                  { Int      $1 }
 | REAL                 { Real     $1 }
 | BOOL                 { Boolean  $1 }
 | STRING               { Literal  $1 }
 | BINDER               { Binder   $1 }
 | IDENT                { Id       $1 }
 | OP                   { Op       $1 }
 
{
happyError [] = HappyAbsSyn4 (Right $ "Unexpected end of input") 
happyError (tk:_) = 
  HappyAbsSyn4 (Right $ "Parse error at: " ++ show (token_pos tk))
}
