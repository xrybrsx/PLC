{
module Grammar where
import Tokens
}

%name parseTree 
%tokentype { Token } 
%error { parseError }

%token 
  import                        { TokenImport _ }
  export                        { TokenExport _ }
  as                            { TokenAs _ }
  insertInto                    { TokenInsert _ }
  select                        { TokenSelect _ }
  endselect                     { TokenEndSelect _ }
  from                          { TokenFrom _ }
  where                         { TokenWhere _ }
  merge                         { TokenMerge _ }
  join                          { TokenJoin _ }
  left                          { TokenLeft _ }
  exists                        { TokenExists _ }
  const                         { TokenConst _ }
  ','                           { TokenComma _ }
  '['                           { TokenLParen _ }
  ']'                           { TokenRParen _ }
  '('                           { TokenLBrac _ }
  ')'                           { TokenRBrac _ }
  '='                           { TokenEquals _ }
  ';'                           { TokenEnd _ }
  var                           { TokenVar _ $$ }
  int                           { TokenInt _ $$ }
  string                        { TokenString _ $$ }

%right insertInto
%right '['
%left ']'
%%

stmts : stmt                                                         { [$1] }
      | stmts stmt                                                   { $2 : $1 }

stmt : Exp ';'                                                       { $1 }

Exp    : var                                                         { Var $1 }
       | int                                                         { PepInt $1 }
       | insertInto Exp Exp                                          { Insert $2 $3 }
       | select '[' ListContents ']' from Exp SelectContinuation     { Select $3 $6 $7 }
       | export Exp                                                  { Export $2 }
       | import Exp as Exp                                           { ImpAs $2 $4 }
       | import Exp                                                  { Imp $2}
       | '(' Exp ')'                                                 { $2 } 

SelectContinuation
  : where Exp '=' Exp  { WhereEqual $2 $4 }
  | where exists Exp   { WhereExists $3 }
  | endselect          { EndSelect }
  | merge Exp          { Merge $2 }
  | join Exp           { Join $2 } 
  | left join Exp      { LeftJoin $3 }
  | join const string  { ConstString (removeQuotes $3) }

ListContents : int                                                   { [$1] }
     | ListContents ',' int                                          { $3 : $1 }

{

parseError :: [Token] -> a
parseError [] = error "Unknown Parse Error" 
parseError (t:ts) = error ("Parse error at line:column " ++ (tokenPosn t))

removeQuotes :: String -> String
removeQuotes (x:xs) = init xs

data Exp = Insert Exp Exp          |
           Select [Int] Exp Exp    | 
           WhereEqual Exp Exp      |
           WhereExists Exp         |
           Where Exp Exp           |
           On Exp                  |
           Merge Exp               |
           Join Exp                |
           LeftJoin Exp            |
           And Exp [Int] Exp [Int] | 
           Export Exp              | 
           Var String              | 
           Imp Exp                 | 
           ImpAs Exp Exp           | 
           PepInt Int              | 
           PepString String        | 
           StringMatrix [[String]] | 
           AddVar Exp              |
           ConstString String      |
           EndSelect     
  deriving (Show, Eq)

}

