{-# LANGUAGE DeriveDataTypeable #-}

import Automata
import CFSM 
import Parser (parseCFSMs)
import FSMParser (parseFSMs, mkSystem)
import DOTParser (parseDOT, mkDSystem)
import LocalType

import PetrifyBridge -- Synthesis
import BuildGlobal -- Synthesis


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
                , synthesis :: Bool
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
                   , synthesis = def 
                                 &= explicit &= name "synthesis"
                                 &=help "Produces a Global Graph /!\\ It might take a while to compute the send projections."
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
               then printAll (cibi pargs) (debug pargs) (noreduce pargs) (synthesis pargs) basename cfsms (head blist)
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


printAll :: Bool -> Bool -> Bool -> Bool -> String -> System -> Int -> IO ()
printAll cibi debug red flag basename cfsms bound = do
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

  printProjectionsDot
    cfsms
    ((syncglobalBase basename 0)++"norm-system.dot")
    ((syncglobalBase basename 0)++"norm-system.png")
    -- putStrLn $ printSystemToGMC cfsms

  
  when flag $ do
    putStrLn "------------- SEND PROJECTIONS -------------------"
    mincfsms <- mapM (\p -> minimisemCRL2 p $ projectTS ts (sendProjection p)) peers
  
    
    let scfsms = M.fromList $ zip peers mincfsms
        fts = if red
              then buildTS 1 scfsms
              else buildReduceTS (isBasic scfsms) 1 scfsms

    printInformation debug cibi red 1 scfsms fts

    -- print all projections of k-TS
    mapM_ (\(x,m) -> writeFile
                     (outputFolder++basename++"-proj-"++(show $ bound)++"-"++(x)++".fsm")
                     (printAutomaton printMLabel $ stringAutomaton m)
          ) $ M.toList scfsms
  
 
    let ssts = synchronise $ buildTS 1 scfsms -- this one cannot be up-to POR
        eventTable = mkEventTable ssts
        newts = integerAutomaton $ projectTS ssts (printMSyncLabel eventTable) 
    writeFile
      (synctsFileName basename bound)
      (printAutomaton id newts)



    writeFile (syncpetriPetrify basename) (ts2petrify eventTable (newts))
    runPetrify (syncpetriPetrify basename) (syncpetriOutput basename)
    buildGlobal (syncpetriOutput basename) (makeReverseTable eventTable) (syncglobalBase basename bound)
    mkPicture ((syncglobalBase basename bound)++"_global.dot") (syncglobalGraph basename bound)

    printProjectionsDot
      scfsms
      ((syncglobalBase basename bound)++"snd-system.dot")
      ((syncglobalBase basename bound)++"snd-system.png")

      
outputFolder = "outputs/"
boundedtsFileName basename b = outputFolder++basename++"-ts-"++(show $ b)++".fsm"
redboundedtsFileName basename b = outputFolder++basename++"-redts-"++(show $ b)++".fsm"
sendtsFileName basename b = outputFolder++basename++"-ts-send-"++(show $ b)++".fsm"
rcvtsFileName basename b = outputFolder++basename++"-ts-rcv-"++(show $ b)++".fsm"
projFileName basename x b = outputFolder++basename++"-proj-"++(show $ b)++"-"++(x)++".fsm"
projrcvFileName  basename x b = outputFolder++basename++"-proj-rcv-"++(show $ b)++"-"++(x)++".fsm" 
projsndFileName  basename x b = outputFolder++basename++"-proj-snd-"++(show $ b)++"-"++(x)++".fsm" 
syncglobalBase basename b =  outputFolder++basename++"-sync-"++(show b)

petriEvtName basename = outputFolder++basename++"-evtTable"++".txt"
petriPetrify basename = outputFolder++basename++"-petrify"++".txt"
petriOutput basename =  outputFolder++basename++"-pn"++".txt"
globalBase basename =  outputFolder++basename
globalGraph basename b = outputFolder++basename++"-"++(show b)++"-globalgraph"++".png"

minimisedSendTS basename = outputFolder++basename++"-min-snd"++".fsm" 

syncglobalGraph basename b = outputFolder++basename++(show b)++"-globalgraph-sync"++".png"
syncpetriOutput basename =  outputFolder++basename++"-pn-sync"++".txt"
syncpetriPetrify basename = outputFolder++basename++"-sync-petrify"++".txt"
synctsFileName basename b = outputFolder++basename++"-ts-sync-"++(show $ b)++".fsm"
syncminimisedSendTS basename = outputFolder++basename++"-min-snd-sync"++".fsm"  


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


compareLTS :: String -> String -> IO Bool
compareLTS s1 s2 =
  let mcmd = "ltscompare -eweak-bisim "++s1++" "++s2
  in do (ec, sd, out) <- readProcessWithExitCode "bash" ["-c",mcmd] []
        -- putStrLn $ show out
        return $ (unpack. strip . pack $ out)=="LTSs are equal (weak bisimilarity)"



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



printProjectionsDot :: System -> String -> String -> IO ()
printProjectionsDot sys file output =
  let dstring = printSystem sys
      cmd =  "dot -Tpng "++file++" -o "++output
  in do writeFile file dstring
        out <- readProcess "bash" ["-c", cmd] []
        return ()
        

        
minimisemCRL2 :: Participant -> (Automaton Configuration (Maybe Label)) -> IO Machine
minimisemCRL2 p aut = do
  writeFile (p++"auto-tmp.fsm") (printAutomaton printMaybeLabel aut)
  runMimimisation (p++"auto-tmp.fsm") (p++"auto-tmp.aut")
  parsemCRL2 (p++"auto-tmp.aut")

runMimimisation :: FilePath -> FilePath -> IO ()
runMimimisation file output =
  let cmd = "ltsconvert -eweak-trace "++file++" "++output
      -- cmd  = "ltsconvert -eweak-bisim "++file++" "++output
  in do out <- readProcess "bash" ["-c", cmd] []
        return ()


 
parsemCRL2 :: FilePath -> IO (Automaton State Label)
parsemCRL2 f = do fcontent <- readFile f
                  let flines = lines fcontent
                      ist =  head $ splitOn "," $ head $ tail $ splitOn "(" (head flines)
                      ntrans = L.map mkTrans $ tail flines         
                  return $ Automaton { states = nub $ concat $ L.map (\(s,(l,t)) -> [s,t]) ntrans
                                     , sinit = ist
                                               --  show $ minimum $ L.map (\x -> read (fst x) :: Int) ntrans
                                     , transitions = ntrans
                                     }
  where mkTrans s = let outersp = splitOn "," (init $ tail $ L.filter (/='"') s)
                        sender = splitOn "TO" (outersp!!1)
                    in case splitOn "SND" (sender!!1) of
                        [x] -> let msg = splitOn "RCV" (sender!!1)
                               in (outersp!!0, ((sender!!0,msg!!0,Receive,msg!!1),outersp!!2))
                        (x:y:xs) -> (outersp!!0, ((sender!!0,x,Send,y),outersp!!2))



runPetrify :: FilePath -> FilePath -> IO ()
runPetrify file output =
  let cmd = "petrify -dead -ip -mints -efc -er "++file++" -o "++output
  in do out <- readProcess "bash" ["-c", cmd] []
        return ()

