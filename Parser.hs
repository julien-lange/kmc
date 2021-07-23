module Parser where

import CFSM
import LocalType

import Data.List as L

-- Parser
import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as T
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Error


mainparser :: Parser [(Participant, LocalType)]
mainparser =  whiteSpace >> sysparser <* eof


parseCFSMs :: String -> Either ParseError [(Participant, LocalType)]
parseCFSMs inp =  parse mainparser "" inp




-- Lexer & Parser
lexer :: T.TokenParser ()
lexer = T.makeTokenParser languageDef


languageDef =
  emptyDef { T.commentStart    = "/*"
           , T.commentEnd      = "*/"
           , T.commentLine     = "--"
           , T.identStart      = lower
           , T.identLetter     = alphaNum
           , T.reservedNames   = ["rec", "end"]
           , T.reservedOpNames = ["!","?"]
           , T.caseSensitive = True
           }

whiteSpace= T.whiteSpace lexer
lexeme    = T.lexeme lexer
symbol    = T.symbol lexer
parens    = T.parens lexer
poperator = T.operator lexer

participantid = (many1 upper)  -- T.identifier lexer -- participant ID
pmessage = T.identifier lexer -- message string
pvariable = T.identifier lexer -- Recursive variable



sysparser :: Parser [(Participant, LocalType)]
sysparser = do { list <- many1 elemparser -- sepBy1 elemparser (char '|' <* spaces)
               ; return list
               } 

elemparser :: Parser (Participant, LocalType)
elemparser = do { id <- participantid
                ; symbol ":"
                --
                ; ty <- ltparser
                --
                 ; return $ (id, ty)
                 }

ltparser :: Parser LocalType
ltparser = do { partner <- participantid
              ; dir <- (symbol "?" <|> symbol "!")
              ; act <-  pmessage
              ; symbol ";"
              ; cont <- ltparser
              ; return $ Act partner (if dir=="!" then Send else Receive) act cont
              }
           <|> 
           do { symbol "rec"
              ; var <-  pvariable
              ; symbol "."
              ; cont <- ltparser
              ; return $ Rec var cont
              }          
           <|> 
           do { symbol "end"
              ; return  End
              }   
           <|> 
           do { var <-  pvariable
              ; return $ Var var
              }
           <|> 
           do { symbol "{"
              ; list <- sepBy1 ltparser (char ',' <* spaces)
              ; symbol "}"
              ; if isWFChoice list
                then return $ Choice (if (isIntChoice list) then Send else Receive) list
                else fail $ "Ill-formed choice"
              } 
           


-- isWFChoice list
