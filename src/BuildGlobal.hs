--
-- Julien Lange <j.lange@ic.ac.uk>
--
module BuildGlobal where

import Data.Map as M
import System.Environment
import System.FilePath.Posix
import PetriNet
import GlobalGraph
import PetrifyBridge


-- main :: IO ()                   -- 
-- main =  do putStrLn "----------------- Global Graph Synthesis -----------------"
--            progargs <- getArgs
--            case progargs of
--              (x:y:z:xs) -> do
--                putStrLn "Parsing PN file..."
--                pnfile <- readFile x
--                let filename = z
--                let pn = parsePetriNet (Prelude.map (\x -> words x) (lines pnfile))
--                evttable <- parseEvtTable y
--                putStrLn "Transformation 1 (One-Source Petri Net)"
--                let one_pn = oneSource pn
--                writeToFile (filename++"_onesourcepn.dot") (printNet evttable one_pn)
--                putStrLn "Transformation 2 (Joined Petri Net)"
--                let joined_pn = (joinPred . joinPost) one_pn
--                writeToFile (filename++"_finalpn.dot") (printNet evttable joined_pn)
--                --
--                putStrLn "Transformation 3 (PN to Pre-Global Graph)"
--                let pregg =  net2globalgraph joined_pn
--                putStrLn "Transformation 4 (Pre-Global Graph to Global Graph)"
--                writeToFile (filename++"_preglobal.dot") (globalGraph2String evttable pregg)
--                let gg = cleanupGG pregg
--                putStrLn $ "Number of Vertices: " ++(show $ nodeNumber gg)
--                putStrLn $ "Number of Transitions: " ++(show $ transNumber gg)
--                writeToFile (filename++"_global.dot") (globalGraph2String evttable gg)
--                putStrLn "Global Graph Synthesis Done."
--                --
--              [] -> do 
--                print "Error: enter a path to a PN file"
               


buildGlobal :: FilePath -> Map String String -> FilePath -> IO ()
buildGlobal petri evttable filename =
  do pnfile <- readFile petri
     let pn = parsePetriNet (Prelude.map (\x -> words x) (lines pnfile))
     -- putStrLn "Transformation 1 (One-Source Petri Net)"
     let one_pn = oneSource pn
     -- writeToFile (filename++"_onesourcepn.dot") (printNet evttable one_pn)
     -- putStrLn "Transformation 2 (Joined Petri Net)"
     let joined_pn = (joinPred . joinPost) one_pn
     -- writeToFile (filename++"_finalpn.dot") (printNet evttable joined_pn)
     --
     -- putStrLn "Transformation 3 (PN to Pre-Global Graph)"
     let pregg =  net2globalgraph joined_pn
     -- putStrLn "Transformation 4 (Pre-Global Graph to Global Graph)"
     -- writeToFile (filename++"_preglobal.dot") (globalGraph2String evttable pregg)
     let gg = cleanupGG pregg
     -- putStrLn $ "Number of Vertices: " ++(show $ nodeNumber gg)
     -- putStrLn $ "Number of Transitions: " ++(show $ transNumber gg)
     writeToFile (filename++"_global.dot") (globalGraph2String evttable gg)
     return ()
     -- putStrLn "Global Graph Synthesis Done."



writeToFile :: FilePath -> String -> IO()
writeToFile file content = writeFile file content
