{-# LANGUAGE DeriveDataTypeable #-}

import Automata
import CFSM 
import Parser (parseCFSMs)
import FSMParser (parseFSMs, mkSystem)
import DOTParser (parseDOT, mkDSystem)
import LocalType

import System.Process
import System.Directory
import System.Console.CmdArgs
import System.Environment
import System.FilePath.Posix
import System.Console.ANSI
import System.IO (stderr)
import System.Process

import Data.List.Split (splitOn)


import Data.Text (strip, pack, unpack)
import Control.Monad
import Text.ParserCombinators.Parsec.Error

-- DEBUG
import System.IO.Unsafe
import Debug.Trace


import Data.Map as M
import Data.List as L

data GenMode = Generate 
             deriving (Data,Typeable,Show,Eq)


data KChecker = KChecker
                { check :: GenMode
                , sysfile :: FilePath
                , bound :: Int
                , upperbound :: Int
                , fsm :: Bool
                , scm :: Bool
                , normal :: Bool
                , noreduce :: Bool
                , cibi :: Bool
                , debug :: Bool
                , minimise :: Bool
                }
              deriving (Data,Typeable,Show,Eq)


submodes = enum
           [Generate
              &= help "Default mode (ignore)"
              &= name "G"
           ]

subargs = KChecker { check = submodes
                   , sysfile = def &= argPos 0 &= typ "FILE"
                   , bound = def &= argPos 1 &= typ "INT" &= opt "1"
                   , upperbound = def &= opt "-1"  &= argPos 2 &= typ "INT"
                   , fsm = def 
                             &= explicit &= name "fsm"
                             &=help "Takes CFSMs as input"
                   , normal = def 
                              &= explicit &= name "normal"
                              &=help "default files"
                   , noreduce = def 
                             &= explicit &= name "noreduce"
                             &=help "Disable partial order reduction of TS (for checking k-MC)"
                   , scm = def 
                             &= explicit &= name "scm"
                             &=help "Takes DOT as input (experimental)"
                   , cibi = def 
                            &= explicit &= name "cibi"
                            &=help "check for CIBI condition if need be"
                   , debug = def 
                             &= explicit &= name "debug"
                             &=help "Print some debug info"
                   , minimise = def 
                                &= explicit &= name "minimise"
                                &=help "Minimise the CFSMs first"
                   }
          &= help "Checks k-Multiparty Compatibility for Communicating Session Automata"



parse :: Bool -> Bool -> String -> Either ParseError System
parse False False ssystem =
  case parseCFSMs ssystem of
   Left err -> Left err
   Right sys -> Right $ M.fromList $ L.map (\(x,y) -> (x, type2Machine x y)) sys
parse True _ ssystem =
  case parseDOT ssystem of
   Left err -> Left err
   Right sys -> Right $ (mkDSystem sys)
parse _ True ssystem =
  case parseFSMs ssystem of
   Left err -> Left err
   Right sys -> Right $ mkSystem sys


       
main :: IO ()
main = do createDirectoryIfMissing False outputFolder
          pargs <- cmdArgs (modes [subargs])
          ssystem <- readFile $ sysfile pargs
          case  parse (scm pargs) (fsm pargs) ssystem of
           Left err -> do print err
                          putStrLn "Are you using the correct flag (--fsm or --normal)? See --help."
           Right sys -> do
             let cfsms = if (minimise pargs) 
                         then M.map (stringAutomaton . minimizeHop) sys
                         else sys
                 basename = dropExtension $ takeFileName $ sysfile pargs
                 blist = if (bound pargs) < (upperbound pargs)
                         then [(bound pargs)..(upperbound pargs)]
                         else [bound pargs]
             -- 
             -- print
             if (length blist) == 1
               then printAll (cibi pargs) (debug pargs) (noreduce pargs) basename cfsms (head blist)
               else iterativeCheck (noreduce pargs) (debug pargs) cfsms blist

                 
iterativeCheck :: Bool -> Bool -> System -> [Int] -> IO ()
iterativeCheck nored d _ [] = putStrLn "I don't know!"
iterativeCheck nored d cfsms (bound:xs) = 
  let ts = if nored
           then buildTS bound cfsms
           else buildReduceTS (isBasic cfsms) bound cfsms
  in do when d $ do putStr $ "Trying with bound "++(show bound)
                    putStrLn $ " (*"++(show $ numberOfStates ts)
                      ++"* states and *"
                      ++(show $ numberOfTransitions ts)++"*"
                      ++" transitions in RTS)"
        if ((isBasic cfsms)
            ||
            (koutputindep bound cfsms ts)
            &&
            ((kinputindep True cfsms ts)
             ||
             (kinputindepChanSize bound cfsms ts)
            )
           )
           &&
           (kexhaustive cfsms ts)
          then do when d $ putStrLn $
                    (show bound)++"-OBI OK, "++
                    (show bound)++"-C/SIBI OK, "++
                    (show bound)++"-exhaustivity OK, "++                
                    "checking for safety..."
                  printResult ((show bound)++"-MC: ") (ksafe cfsms ts)
          else iterativeCheck nored d cfsms xs


printAll :: Bool -> Bool -> Bool -> String -> System -> Int -> IO ()
printAll cibi debug red basename cfsms bound = do
  let ts = if red
           then buildTS bound cfsms
           else buildReduceTS (isBasic cfsms) bound cfsms
      peers = M.keys cfsms
  when debug $ putStrLn $ (show $ numberOfStates ts)
                      ++" states, "
                      ++(show $ numberOfTransitions ts)
                      ++" transitions in RTS, and "
                      ++(show $ length cfsms)
                      ++" machines in system"
  --
  -- Safety checks
  --
  printInformation debug cibi red bound cfsms ts
  writeFile
    (boundedtsFileName basename bound)
    (printAutomaton printLabel ts)


      
outputFolder = "outputs/"
boundedtsFileName basename b = outputFolder++basename++"-ts-"++(show $ b)++".fsm"
redboundedtsFileName basename b = outputFolder++basename++"-redts-"++(show $ b)++".fsm"
sendtsFileName basename b = outputFolder++basename++"-ts-send-"++(show $ b)++".fsm"
rcvtsFileName basename b = outputFolder++basename++"-ts-rcv-"++(show $ b)++".fsm"
projFileName basename x b = outputFolder++basename++"-proj-"++(show $ b)++"-"++(x)++".fsm"
projrcvFileName  basename x b = outputFolder++basename++"-proj-rcv-"++(show $ b)++"-"++(x)++".fsm" 
projsndFileName  basename x b = outputFolder++basename++"-proj-snd-"++(show $ b)++"-"++(x)++".fsm" 
syncglobalBase basename b =  outputFolder++basename++"-sync-"++(show b)


printInformation :: Bool -> Bool -> Bool -> Int -> System -> TS -> IO ()
printInformation debug cibi flag bound cfsms ts =
  do printResult "CSA: " (isCSA cfsms)
     let prex = if flag then "" else "reduced "
         sibi = kinputindep True cfsms ts
     if isBasic cfsms
       then printResult "Basic: " True
       else do printResult (prex++(show bound)++"-OBI: ") $ koutputindep bound cfsms ts
               when debug $
                 printResult (prex++(show bound)++"-IBI: ") $ kinputindep False cfsms ts
             
               printResult (prex++(show bound)++"-SIBI: ") $ sibi
               when (cibi && not sibi) $ do
                 printResult (prex++(show bound)++"-CIBI: ") $ kinputindepChanSize bound cfsms ts
     printResult (prex++(show bound)++"-exhaustive: ") (kexhaustive cfsms ts)
     printResult (prex++(show bound)++"-safe: ") (ksafe cfsms ts)





printResult :: String -> Bool -> IO()
printResult s t = do putStr $ s
                     selectColor t
                     putStrLn $ show t
                     setSGR [Reset]
                     hSetSGR stderr [Reset]

selectColor :: Bool -> IO()
selectColor True =  setSGR [SetColor Foreground Vivid Green]
selectColor False =  setSGR [SetColor Foreground Vivid Red]




mkPicture :: FilePath -> FilePath -> IO ()
mkPicture file output =
  let cmd = "dot -Tpng "++file++" -o "++output
  in do out <- readProcess "bash" ["-c", cmd] []
        return ()

