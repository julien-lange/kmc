module LocalType where

import CFSM
import Automata

import Data.List as L
import Data.Map as M
import Data.Char (toUpper)

data LocalType = Act Participant Direction Message LocalType
               | Rec String LocalType
               | Var String      
               | End
               | Choice Direction [LocalType]
               deriving (Eq, Ord, Read, Show)
                        
isSend :: LocalType -> Bool
isSend (Act _ Send s lt) = True
isSend _ = False

isReceive :: LocalType -> Bool
isReceive (Act _ Receive _ lt) = True
isReceive _ = False


partners :: LocalType -> [Participant]
partners (Act p _ _ _) = [p]
partners _ = []

isExtChoice :: [LocalType] -> Bool
isExtChoice [] = False
isExtChoice (x:xs) = isReceive x

isIntChoice :: [LocalType] -> Bool
isIntChoice [] = False
isIntChoice (x:xs) = isSend x


isWFChoice :: [LocalType] -> Bool
isWFChoice [] = False
isWFChoice xs = True
-- isWFChoice xs = ((and (L.map isSend xs))  || (and (L.map isReceive xs)))
--                 &&
--                 (length $ concat $ L.map partners xs) == 1



----- TRANSLATION FROM LOCALTYPE TO CFSM -------------------
naming :: Machine -> Map State State
naming m = M.fromList $ snd $ mapAccumL (\x y -> (x+1,(y,show x))) 0 (states m)


stToUpper :: String -> String
stToUpper = L.map toUpper

genState :: String -> (Map String State) -> LocalType -> State
genState s map (Var x) = case M.lookup x map of 
  Just y -> y
  Nothing -> error $ "ill formed type"
genState s map (Rec s' t) = genState s map t 
genState s map t = s


type2Machine :: Participant -> LocalType -> Machine
type2Machine pid t = let nedges = L.nub $ genEdges ninit M.empty [] t
                         nstates =  L.nub $ ninit:(L.foldr (++) [] $ L.map (\(s,(m,t)) -> [s,t]) nedges)
                         ninit = genState (stToUpper (pid++"o")) M.empty t
                         tmpmachine = Automaton { states = nstates
                                                , sinit = ninit
                                                , transitions = nedges
                                                }
                     in rename (naming tmpmachine) tmpmachine
                   
                     
                  
  where genEdges prev map acc (Rec s t) = genEdges prev (M.insert s prev map) acc t
        genEdges prev map acc (Act partner dir (s,pl) t) = 
          let next = genState ((stToUpper prev)++(stToUpper s)++(stToUpper pl)) map t
              lab = if dir == Send
                    then (pid, partner, dir, (s,pl))
                    else (partner, pid, dir, (s,pl))
          in (prev, (lab, next)):(genEdges next map acc t)
        genEdges prev map acc (Choice dir xs) = L.foldr (++) [] (L.map (genEdges prev map acc) xs)
        genEdges prev map acc (End) = []
        genEdges prev map acc (Var x) = []



rename :: (Map State State) -> Machine -> Machine
rename names m = let nedges = L.nub $ L.map (\(s,(l,t)) -> (names!s,(l,names!t))) $ transitions m
                     nodes = L.nub $ L.map (\x -> names!x) $ states m
                     ninit = names ! sinit m
           in Automaton
                       { states = nodes
                       , sinit = ninit
                       , transitions = nedges
                       }
