module DOTParser where

import CFSM
import Automata

import Data.List as L
import Data.Map as M
import Data.Maybe
import Data.List.Split (splitOn)


-- Parser
import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as T
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Error


-- DEBUG
import System.IO.Unsafe
import Debug.Trace

type PTransition = (String, ((String, Direction, (String, String)), String))
type PMachine = (String, String, [PTransition])
type PSystem = [PMachine]

mainparser :: Parser PSystem
mainparser =  whiteSpace >> systemparser <* eof


parseDOT :: String -> Either ParseError PSystem
parseDOT inp =  parse mainparser "" inp



mkCFSM :: PMachine -> Machine
mkCFSM (id, init, trans) = Automaton { states = nub $ concat $ L.map (\(x,(y,z)) -> [x,z]) ntrans
                                     , sinit = init
                                     , transitions = ntrans
                                     }
  where ntrans = L.map (\(x,(y,z)) -> (x, (mklabel y, z))) trans
        mklabel (p,d,m) = if d == Send
                          then (id, p, d, truncmsg m)
                          else (p, id, d, truncmsg m)
        truncmsg m = head $ splitOn "(" m

mkDSystem :: PSystem -> System
mkDSystem sys = M.fromList $ L.map (\(n,x,y) -> (n, mkCFSM (n,x,y))) sys



-- Lexer & Parser
lexer :: T.TokenParser ()
lexer = T.makeTokenParser languageDef


languageDef =
  emptyDef { T.commentStart    = "/*"
           , T.commentEnd      = "*/"
           , T.commentLine     = "--"
           , T.identStart      = alphaNum
           , T.identLetter     = alphaNum
           , T.reservedNames   = ["!","?","]","[","->"]
           , T.reservedOpNames = ["!","?","]","[","->"]
           , T.caseSensitive = True
           }

whiteSpace= T.whiteSpace lexer
lexeme    = T.lexeme lexer
symbol    = T.symbol lexer
parens    = T.parens lexer
poperator = T.operator lexer

participantid = T.identifier lexer  -- T.identifier lexer -- participant ID

pstate = T.identifier lexer


labmessage = do { lab <- many1 (alphaNum <|> oneOf "(:)|&, \\@=-_{}")
                ; return (lab, "")
                }
pmessage = do { lab <- T.identifier lexer
              ; symbol "<"
              ; pl <- T.identifier lexer
              ; symbol ">"
              ; return (lab, pl)
              }

 
transitionParser :: Parser (Maybe PTransition)
transitionParser = do { src <- pstate
                      ; symbol "->"
                      ; trg <- pstate
                      ; symbol "[ label="
                      ; partner <- participantid
                      ; dir <- (symbol "?" <|> symbol "!")
                      ; msg <- choice [try pmessage, try labmessage]
                      ; symbol  "];"
                      ; return $ Just (src, ((partner, if dir=="!" then Send else Receive, msg), trg))
                      }

emptyLineParser :: Parser (Maybe PTransition)
emptyLineParser = do { src <- pstate
                     ; symbol "["
                     ; symbol "label="
                     ; n <- pstate
                     ; symbol ":"
                     ; msg <- option "" pmessage
                     ; symbol  "];"
                     ; return Nothing
                     }

machineparser :: Parser PMachine
machineparser = do { symbol "digraph"
                   ; symbol "G"
                   ; symbol "{"
                   ; symbol "NAME"
                   ; name <- participantid
                   ; symbol ";"
                   ; symbol "compound = true;"
                   ; list <- many1 (choice [try emptyLineParser, transitionParser])
                   ; symbol "}"
                   ; let msgs = catMaybes list
                     in return (name, getInitState msgs, msgs)
                   }

getInitState :: [PTransition] -> String
getInitState xs = show $ minimum $ L.map (\x -> read (fst x) :: Int) xs
                
systemparser :: Parser PSystem
systemparser = do { list <- many1 machineparser
                  ; return list
                  }



-- isWFChoice list
