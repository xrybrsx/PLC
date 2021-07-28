{ 
module Tokens where 
}

%wrapper "posn" 
$digit = 0-9     
$alpha = [a-zA-Z]    

tokens :-
  $white+                            ;
  "--".*                             ;
  import                             { \p s -> TokenImport p }
  as                                 { \p s -> TokenAs p }
  \.                                 { \p s -> TokenDot p }
  \,                                 { \p s -> TokenComma p }
  insert $white+ into                { \p s -> TokenInsert p }
  select                             { \p s -> TokenSelect p }
  endselect                          { \p s -> TokenEndSelect p }
  from                               { \p s -> TokenFrom p }
  where                              { \p s -> TokenWhere p }
  merge                              { \p s -> TokenMerge p }
  join                               { \p s -> TokenJoin p }
  left                               { \p s -> TokenLeft p }
  exists                             { \p s -> TokenExists p }
  const                              { \p s -> TokenConst p }
  \[                                 { \p s -> TokenLParen p }
  \]                                 { \p s -> TokenRParen p }
  \(                                 { \p s -> TokenLBrac p }
  \)                                 { \p s -> TokenRBrac p }
  \=                                 { \p s -> TokenEquals p }
  \'                                 { \p s -> TokenQuote p }
  export                             { \p s -> TokenExport p }
  \;                                 { \p s -> TokenEnd p }
  [$alpha] [$alpha $digit \_ \’]*    { \p s -> TokenVar p s }
  $digit+                            { \p s -> TokenInt p (read s) }
  \' [$alpha $digit]* \'             { \p s -> TokenString p s }
  
{

data Token = 
    TokenImport AlexPosn        |
    TokenAs AlexPosn            |
    TokenDot AlexPosn           |
    TokenComma AlexPosn         |
    TokenInsert AlexPosn        |
    TokenSelect AlexPosn        |
    TokenEndSelect AlexPosn     |
    TokenWhere AlexPosn         |
    TokenLParen AlexPosn        |
    TokenRParen AlexPosn        |
    TokenLBrac AlexPosn         |
    TokenRBrac AlexPosn         |
    TokenExport AlexPosn        |
    TokenEnd AlexPosn           |
    TokenMerge AlexPosn         |
    TokenJoin AlexPosn          |
    TokenLeft AlexPosn          |
    TokenExists AlexPosn        |
    TokenConst AlexPosn         |
    TokenEquals AlexPosn        |
    TokenQuote AlexPosn         |
    TokenVar AlexPosn String    |      
    TokenString AlexPosn String |      
    TokenInt AlexPosn Int       |
    TokenFrom AlexPosn
    deriving(Eq, Show)

tokenPosn :: Token -> String
tokenPosn (TokenImport (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenAs  (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenDot  (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenComma  (AlexPn a l c) ) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenInsert  (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenSelect  (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenEndSelect  (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenLParen (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenRParen (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenLBrac (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenRBrac (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenWhere (AlexPn a l c) ) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenExport (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenEnd (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenVar (AlexPn a l c) _ ) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenString (AlexPn a l c) _ ) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenInt (AlexPn a l c) _ ) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenFrom (AlexPn a l c) ) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenEquals (AlexPn a l c) ) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenQuote (AlexPn a l c) ) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenMerge (AlexPn a l c) ) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenJoin (AlexPn a l c) ) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenLeft (AlexPn a l c) ) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenExists (AlexPn a l c) ) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenConst (AlexPn a l c) ) = show(l) ++ ":" ++ show(c)

}