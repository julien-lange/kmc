module PetrifyBridge where

import Automata
import CFSM

import Data.List as L
import Data.Map as M
import Data.Maybe (catMaybes)

import Data.List.Split 
import Data.Char


int2string :: Int -> String
int2string i = L.foldl (++) "" (convert (L.map digitToInt $ show i))
  where 
    convert (x:xs) = (single x):(convert xs)
    convert [] = []
    single x = case x of
      0 -> "zero"
      1 -> "one"
      2 -> "two"
      3 -> "three"
      4 -> "four"
      5 -> "five"
      6 -> "six"
      7 -> "seven"
      8 -> "eight"
      9 -> "nine"
      a -> error $ "ERROR:"++(show a)

mkEventTable :: Automaton a Label -> Map Label String
mkEventTable aut = M.fromList $ snd $
                   mapAccumL (\x y -> (x+1,(y, int2string x))) 0 $
                   nub $ L.map (fst . snd) $ transitions aut


printGlobalLabel :: Label -> String
printGlobalLabel (p,q,d,(m,pl)) = p++"->"++q++":"++m++pl
  -- case d of
  --  Send -> p++q++"!"++m
  --  Receive -> p++q++"?"++m


 
printEventTable :: Map Label String -> String
printEventTable map = helper (M.toList map)
  where helper xs = intercalate "\n" $ L.map (\(x,y) -> y++", "++(printGlobalLabel x)) xs



makeReverseTable :: Map Label String -> Map String String
makeReverseTable map = M.fromList $ L.map (\(x,y) -> (y,printGlobalLabel x)) $ M.toList map

parseEvtTable :: FilePath -> IO (Map String String)
parseEvtTable file =
  do table <- readFile file
     let list = Prelude.map words $ lines table
     let tab = M.fromList $ 
               L.map (\x -> let res = splitOn "," (unwords x) in (head res, unwords $ tail res) ) list
     return tab

printState :: Int -> String
printState i = "q"++(int2string i)

-- ts2petrify :: Map Label String -> (Automaton Int Label) -> String
-- ts2petrify map aut =
--   let st_nodes = ".outputs "++ (intercalate " " $ M.elems map)++"\n"
--       st_init = ".marking {"++(printState $ sinit aut)++"}\n"
--       st_trans = intercalate "\n"  $
--                  L.map (\(x,(y,z)) -> (printState x)
--                                       ++" "++
--                                       (map!y)
--                                       ++" "++
--                                       (printState z)
--                        )
--                  (transitions aut)
--   in st_nodes++".state graph \n"++st_trans++"\n"++st_init++".end\n"


ts2petrify :: Map Label String -> (Automaton Int String) -> String
ts2petrify map aut =
  let st_nodes = ".outputs "++ (intercalate " " alpha)++"\n"
      alpha = nub $ L.map (\(x,(y,z)) -> y) $ transitions aut
      st_init = ".marking {"++(printState $ sinit aut)++"}\n"
      st_trans = intercalate "\n"  $
                 L.map (\(x,(y,z)) -> (printState x)
                                      ++" "++
                                      (y)
                                      ++" "++
                                      (printState z)
                       )
                 (transitions aut)
  in st_nodes++".state graph \n"++st_trans++"\n"++st_init++".end\n"
     


parseFSM :: FilePath -> IO (Automaton Int String)
parseFSM file = helper
  where helper = do content <- readFile file
                    let list = Prelude.map words $ lines content
                        ntrans = L.map (\(x,(y,z)) -> (read x :: Int, (init . tail $ y, read z :: Int))) $
                                 L.map (\x -> let res = splitOn " " (unwords x) in (res!!0, (res!!2, res!!1))) $
                                 L.filter (["---"] /=) list
                    return Automaton { states = concat $ L.map (\(s,(l,t)) -> [s,t]) ntrans
                                     , sinit = 1
                                     , transitions = ntrans
                                     }
