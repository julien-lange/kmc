module FSMParser where

import CFSM
import Automata

import Data.List as L
import Data.Map as M

-- Parser
import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as T
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Error


-- DEBUG
import System.IO.Unsafe
import Debug.Trace

type PTransition = (String, ((String, Direction, (String, String)), String))
type PMachine = (String, [PTransition])
type PSystem = [PMachine]

mainparser :: Parser PSystem
mainparser =  whiteSpace >> systemparser <* eof


parseFSMs :: String -> Either ParseError PSystem
parseFSMs inp =  parse mainparser "" inp



mkCFSM :: String -> PMachine -> Machine
mkCFSM id (init, trans) = Automaton { states = nub $ concat $ L.map (\(x,(y,z)) -> [x,z]) ntrans
                                    , sinit = init
                                    , transitions = ntrans
                                    }
  where ntrans = L.map (\(x,(y,z)) -> (x, (mklabel y, z))) trans
        mklabel (p,d,m) = if d == Send
                          then (id, p, d, m)
                          else (p, id, d, m)


mkSystem :: PSystem -> System
mkSystem sys = M.fromList $ L.map (\(x,y) -> (show x, mkCFSM (show x) y)) nsys
  where nsys = snd $ mapAccumL (\x y -> (x+1,(x,y))) 0 sys



-- Lexer & Parser
lexer :: T.TokenParser ()
lexer = T.makeTokenParser languageDef


languageDef =
  emptyDef { T.commentStart    = "/*"
           , T.commentEnd      = "*/"
           , T.commentLine     = "--"
           , T.identStart      = alphaNum
           , T.identLetter     = alphaNum
           , T.reservedNames   = [".outputs", ".state", "graph", ".marking", ".end"]
           , T.reservedOpNames = ["!","?"]
           , T.caseSensitive = True
           }

whiteSpace= T.whiteSpace lexer
lexeme    = T.lexeme lexer
symbol    = T.symbol lexer
parens    = T.parens lexer
poperator = T.operator lexer

participantid = T.identifier lexer  -- T.identifier lexer -- participant ID
pstate = T.identifier lexer



labmessage = do { lab <- T.identifier lexer
                ; return (lab, "")
                }
pmessage = do { lab <- T.identifier lexer
              ; symbol "<"
              ; pl <- T.identifier lexer
              ; symbol ">"
              ; return (lab, pl)
              }


 
transitionParser :: Parser PTransition
transitionParser = do { src <- pstate
                      ; partner <- participantid
                      ; dir <- (symbol "?" <|> symbol "!")
                      ; msg <- choice [try pmessage, try labmessage]
                      ; trg <- pstate
                      ; return $ (src, ((partner, if dir=="!" then Send else Receive, msg), trg))
                      }


machineparser :: Parser PMachine
machineparser = do { symbol ".outputs"
                   ; symbol ".state graph"
                   ; list <- many1 transitionParser
                   ; symbol ".marking"
                   ; init <- pstate
                   ; symbol ".end"
                   ; return (init, list)
                   }
                
systemparser :: Parser PSystem
systemparser = do { list <- many1 machineparser
                  ; return list
                  }



-- isWFChoice list
