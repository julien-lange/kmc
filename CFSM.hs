module CFSM where

import Data.List as L
import Data.Map as M
import Data.Set as S
import Data.Function (on)
import Automata

import Control.Parallel.Strategies
import Control.Parallel

-- DEBUG
import System.IO.Unsafe
import Debug.Trace


type Participant = String
type State = String
type Message = (String, String) -- (Label, Payload)
data Direction = Send
               | Receive
               deriving (Show, Eq, Ord, Read)
type Label = (Participant, Participant, Direction, Message)


type System = Map Participant Machine

type StateMap = Map Participant State
type QueueMap = Map (Participant, Participant) [Message]

type Configuration = ((StateMap, QueueMap), [Label])

type Machine = Automaton State Label

type TS = Automaton Configuration Label



dualDir :: Direction -> Direction -> Bool
dualDir Send Receive = True
dualDir Receive Send = True
dualDir _ _ = False

dual :: Label -> Label -> Bool
dual a@(s, r, d, m) b@(s', r', d', m')
  | s==s' && (r==r') && (m==m') = dualDir d d'
  | otherwise = False



mkDual :: Label -> Label
mkDual (s, r, Send, msg) = (s, r, Receive, msg)
mkDual (s, r, Receive, msg) = (s, r, Send, msg)

direction :: Label -> Direction
direction (s, r, Send, msg) = Send
direction (s, r, Receive, msg) = Receive

channel :: Label -> (Participant, Participant)
channel (s, r, d, msg) = (s,r)



subject :: Label -> Participant
subject (s, r, Send, msg) = s
subject (s, r, Receive, msg) = r

object :: Label -> Participant
object (s, r, Send, msg) = r
object (s, r, Receive, msg) = s

printLabel :: Label -> String
printLabel (s, r, Send, (msg,pl)) = s++r++"snd"++msg++"<"++pl++">"
printLabel (s, r, Receive, (msg,pl)) = s++r++"rcv"++msg++"<"++pl++">"

printMLabel :: Label -> String
printMLabel l = printLabel l



printMaybeLabel :: Maybe Label -> String
printMaybeLabel (Just (s, r, d, (msg,pl))) = case d of
                                         Send ->  s++"TO"++r++"SND"++msg++"_"++pl
                                         Receive ->  s++"TO"++r++"RCV"++msg++"_"++pl
printMaybeLabel Nothing = "tau"

printMSyncLabel :: Map Label String -> Label -> String
printMSyncLabel map l = map!l



printMSLabel :: Map Label String -> Maybe Label -> String
printMSLabel map (Just l) = map!l
printMSLabel map Nothing = "tau"

initConf :: System -> Configuration
initConf sys = let emptyQ = M.fromList $ [((x,y),[]) | x <- keys sys, y <- keys sys, x/=y]
                   initS = M.map sinit sys
               in (initS, emptyQ)

                   
nextConfiguration :: Int -> System -> Configuration -> [(Label, Configuration)]
nextConfiguration k sys (states, queues) =
  let nonemptyqueues = L.filter (\(x,y) -> not $ L.null y) $ M.toList queues
      nonfullqueues = L.filter (\(x,y) -> (length y) < k) $ M.toList queues
      partitionEdges m x = L.partition (\((s,t,d,m),ns) -> d==Send) $ outedges m x
      nexttransitions = (L.map (\(id, m) -> partitionEdges m (states!id)) $ M.toList sys) 
      allsends = concat $ L.map fst nexttransitions
      allrecvs = concat $ L.map snd nexttransitions
      --
      sent = [(((s,r),(q++[m])), (s,t), (s,r,d,m)) | ((s',r'),q) <- nonfullqueues
                                                   , ((s,r,d,m),t) <- allsends, s'==s, r==r']
             
      received = [(((s,r),(tail q)), (r,t),(s,r,d,m))  | ((s',r'),q) <- nonemptyqueues
                                                       , ((s,r,d,m),t) <- allrecvs
                                                       , s'==s, r==r', head q ==m]
      --
      updateQueue qs ((s,r),q) = M.insert (s,r) q qs
      updateStates st (p,t) = M.insert p t st
  in L.map (\(x,y,z) -> (z, (updateStates states y, updateQueue queues x))) (sent++received)


nextConfigurationFixed :: Int -> System -> Configuration -> Label -> Configuration
nextConfigurationFixed k sys (states, queues) l@(s, r, Send, msg) =
  let oldqueue = queues M.! (s,r)
      target = successor (sys M.! s) (states M.! s) l
  in (M.insert s target states, M.insert (s,r) (oldqueue++[msg]) queues)
nextConfigurationFixed k sys (states, queues) l@(s, r, Receive, msg) =
  let (x:xs) = queues M.! (s,r)
      target = successor (sys M.! r) (states M.! r) l
   in (M.insert r target states, M.insert (s,r) xs queues)

buildTS :: Int -> System -> TS
buildTS k sys = Automaton { states = nub $ concat $ L.map (\x-> [fst x, snd . snd $ x]) alltrans
                          , sinit = initConf sys
                          , transitions = alltrans
                          }
  where alltrans = nub $ helper [] [] [initConf sys]
        helper acc seen [] = acc
        helper acc seen (x:xs)
          | x `elem` seen = helper acc seen xs
          | otherwise =
              let next = nextConfiguration k sys x
                  ntrans = L.map (\(l,t) -> (x,(l,t))) next
              in helper (acc++ntrans) (x:seen) (xs++(L.map snd next))



compareTransitions :: [(Label, Configuration)] -> [(Label, Configuration)] -> Ordering
compareTransitions [] _ = LT
compareTransitions _ [] = GT
compareTransitions ((x,c):xs) ((y,d):ys)
  | (direction x == direction y) = (compare `on` length) xs ys
  | (direction x == Send) = GT
  | (direction x == Receive) = LT

buildReduceTS :: Bool -> Int -> System -> TS
buildReduceTS basic k sys = Automaton { states = nub $ concat $ L.map (\x-> [fst x, snd . snd $ x]) ntrans
                                         , sinit = initConf sys
                                         , transitions = ntrans
                                         }
  where f (p1,p2,Send,m) (q1,q2,Send,m') = (p1==q1) 
        f (p1,p2,Receive,m) (q1,q2,Receive,m') = (p2==q2)
        f (p1,p2,Send,m) (q1,q2,Receive,m') = (p1==q2)
        f (p1,p2,Receive,m) (q1,q2,Send,m') = (p2==q1) 

        ntrans = helper [(initConf sys, [])] [] []
        helper [] seen acc =  acc
        helper ((s,todo):ss) seen acc
          | s `L.elem` seen = helper ss seen acc
          | otherwise =
              case todo of
              [] -> case sortBy compareTransitions $
                         -- sortBy (compare `on` length) $
                         L.groupBy (\x y -> f (fst x) (fst y) ) $ nextConfiguration k sys s of
                     [] -> helper ss seen acc
                     (h:xs) -> let current = (L.map (\(x,y) -> (s,(x,y))) h)
                                   next = L.map (\(x,y) -> (y,xs)) h
                               in helper (ss++next) (s:seen) (acc++current)
        
              (list:rest) -> let current = (L.map (\(x,y) -> (s,(x, (nextConfigurationFixed k sys s x)))) list)
                                 next =  L.map (\(x,y) -> (nextConfigurationFixed k sys s x,rest)) list
                             in helper (ss++next) (s:seen) (acc++current)
        
projectLabel :: Participant -> Label -> Maybe Label
projectLabel p (s,r,dir,msg)
  | dir == Send && s==p = Just (s,r,dir,msg)
  | dir == Receive && r==p = Just (s,r,dir,msg)
  | otherwise = Nothing



projectMLabel :: Direction -> Participant -> Label -> Maybe Label
projectMLabel dir p l = projectLabelDir dir p l


projectLabelDir :: Direction -> Participant -> Label -> Maybe Label
projectLabelDir Send p (s,r,dir,msg)
  | dir == Send && s==p = Just (s,r,dir,msg)
  | dir == Send && r==p = Just (s,r,Receive,msg)
  | otherwise = Nothing 
projectLabelDir Receive p (s,r,dir,msg)
  | dir == Receive && s==p = Just (s,r,Send,msg)
  | dir == Receive && r==p = Just (s,r,dir,msg)
  | otherwise = Nothing

sendProjection :: Participant -> Label -> Maybe Label
sendProjection p (s,r,Send,msg)
  | s == p = Just (s,r,Send,msg)
  | r == p = Just (s,r,Receive,msg)           
  | otherwise = Nothing               
sendProjection p (s,r,Receive,msg) = Nothing
  
projectDirection :: Direction -> Label -> Maybe Label
projectDirection dir (s,r,d,msg)
  | dir == d = Just (s,r,d,msg)
  | otherwise = Nothing


projectTS :: Automaton a b -> (b -> c) -> Automaton a c
projectTS ts projlab = Automaton { states = states ts
                                 , sinit = sinit ts
                                 , transitions = L.map (\(x,(y,z)) -> (x, (projlab y, z))) $ transitions ts
                                 }
                 


reduceTS :: TS -> TS
reduceTS ts =
  let fun (p1,p2,Send,m) (q1,q2,Send,m') = (p1==q1)
      fun (p1,p2,Receive,m) (q1,q2,Receive,m') = (p2==q2)
      fun _ _ = False
  in reduce fun ts



kexhaustive :: System -> TS -> Bool
kexhaustive sys ts = L.and $ (parMap rpar) helper (states ts)
  where helper (ss,qq) = let msend m = L.map fst $ L.filter (\x -> (direction $ fst x) == Send) $ successors (sys!m) (ss!m)
                             asends = concat $ L.map msend $ M.keys sys
                             --
                             fun a b = a==b 
                             --
                             gun a b = (subject a) /= (subject b)
                         in L.and $ L.map (\x -> findLabel ts (ss,qq) (fun x) (gun x) ) asends



ksafe :: System -> TS -> Bool
ksafe sys ts =  L.and $ (parMap rpar) helper (states ts)
  where helper (ss,qq) = let mrcvs m = L.map fst $ L.filter (\x -> (direction $ fst x) == Receive) $ successors (sys!m) (ss!m)
                             arcvs = concat $ L.map mrcvs $ M.keys sys
                             
                             fun (s, r, Receive, msg) (p,q,d,m) = (r==q) && (d==Receive)
                             fun _ _ = False

                             qs = L.map (\((s,r),msgs) -> (s, r, Receive, head msgs)) $
                                  L.filter (\x -> not $ L.null $ snd x) $ M.toList qq
                             
                         in (L.and $ L.map (\x -> findLabel ts (ss,qq) (fun x) (\x->True)) arcvs) -- progress
                            &&
                            (L.and $ L.map (\x -> findLabel ts (ss,qq) (x==) (\x->True)) qs) -- evt reception
              

koutputindep :: Int -> System -> TS -> Bool
koutputindep bound sys ts =  L.and $ (parMap rpar) helper (states ts)
  where helper (ss,qq) = koutputindepConfiguration bound sys (ss,qq)

koutputindepConfiguration :: Int -> System -> Configuration -> Bool
koutputindepConfiguration bound sys (ss,qq) =
  let nextconfs = nextConfiguration bound sys (ss,qq)  
      msend m = L.map fst
                $ L.filter (\x -> (direction $ fst x) == Send)
                $ successors (sys!m) (ss!m)
      actsend m = L.map fst
                  $ L.filter (\x -> (direction $ fst x) == Send
                                    && ((subject $ fst x) == m))
                  $ nextconfs
  in L.and $ L.map (\x ->
                     (L.null $ actsend x)
                          ||
                          (
                              (S.fromList $ msend x)
                              ==
                              (S.fromList $ actsend x)
                          )
                   ) $ M.keys sys

     
kinputindep :: Bool -> System -> TS -> Bool
kinputindep sibi sys ts = L.and $ (parMap rpar) helper (states ts)
  where helper (ss,qq) = let mreceive m = L.map fst
                                          $ L.filter (\x -> (direction $ fst x) == Receive)
                                          $ successors (sys!m) (ss!m)
                             --
                             actrcv m = nub $ L.map fst
                                        $ L.filter (\x -> (direction $ fst x) == Receive
                                                          && ((subject $ fst x) == m))
                                        $ successors ts (ss,qq)
                             --
                             asends =  concat
                                       $ L.map (\x -> if (not $ L.null $ actrcv x)
                                                      then avlabel (head $ actrcv x) (mreceive x)
                                                      else []
                                               ) $ M.keys sys
                             --
                             avlabel a [] = []
                             avlabel a (b:bs) =
                               if (object a /= object b)
                               then (mkDual b):(avlabel a bs)
                               else avlabel a bs
                             --
                         in  (L.and $ L.map (\x -> (length $ actrcv x) < 2)$ M.keys sys)
                             &&
                             (
                               (not sibi)
                               ||
                               (L.and $ L.map (\x -> not $ findLabel ts (ss,qq) (x==) (\x->True) ) asends)
                             )

kinputindepChanSize :: Int -> System -> TS -> Bool
kinputindepChanSize bound sys ts = L.and $ (parMap rpar) helper (states ts)
  where helper (ss,qq) = let mreceive m = L.map fst
                                          $ L.filter (\x -> (direction $ fst x) == Receive)
                                          $ successors (sys!m) (ss!m)
                             --
                             actrcv m = nub $ L.map fst
                                        $ L.filter (\x -> (direction $ fst x) == Receive
                                                          && ((subject $ fst x) == m))
                                        $ successors ts (ss,qq)
                             --
                             asends = concat $
                                      L.map (\x -> if (not $ L.null $ actrcv x)
                                                   then case avlabel (head $ actrcv x) (mreceive x) of
                                                         [] -> []
                                                         (z:zs) -> [(head $ actrcv x, (z:zs))]
                                                   else []
                                            ) $ M.keys sys
                             --
                             avlabel a [] = []
                             avlabel a (b:bs) =
                               if (object a /= object b)
                               then (mkDual b):(avlabel a bs)
                               else avlabel a bs
                             --
                         in (L.and $ L.map (\x -> (length $ actrcv x) < 2)$ M.keys sys)
                            &&
                            (
                              (L.and $ L.map
                               (\(x,ys) -> findDependence
                                           ts
                                           (successor ts (ss,qq) x)
                                           x
                                           dependence
                                           (\z -> z `L.elem` ys)) asends)
                            )                       

isBasic :: System -> Bool
isBasic sys = L.and $ (parMap rpar) testMachine (M.elems sys)
  where testMachine m = L.and $ L.map (testState m) $ states m
        testState m s = let sc = successors m s
                            (chans,dirs) = unzip $ L.map (\((p,q,d,m),_) -> ((p,q), d)) sc
                        in (L.null sc)
                           ||
                           (
                             (L.length (nub chans) == 1) && (L.length (nub dirs) == 1)
                           )


isCSA :: System -> Bool
isCSA sys = L.and $ (parMap rpar) testMachine (M.elems sys)
  where testMachine m = L.and $ L.map (testState m) $ states m
        testState m s = let sc = successors m s
                            (chans,dirs) = unzip $ L.map (\((p,q,d,m),_) -> ((p,q), d)) sc
                        in (L.null sc)
                           ||
                           (
                             (L.length (nub dirs) == 1)
                           )


printSystemToGMC :: System -> String
printSystemToGMC sys =
  let envi = M.fromList $ snd $ mapAccumL (\x y -> (x+1, (y,x))) 0 (M.keys sys)
  in intercalate "\n\n" $ L.map ((printMachineToGMC envi) . integerAutomaton) $ M.elems sys

printMachineToGMC :: (Map String Int) -> (Automaton Int Label)  -> String
printMachineToGMC envi aut = ".outputs\n" 
                             ++".state graph\n"
                             ++(intercalate "\n" strans)++"\n"
                             ++".marking "++(pstate $ sinit aut)++"\n"
                             ++".end"
  where strans = L.map (\(src, (lab, trg)) -> (pstate src) ++" "++(plab lab)++" "++(pstate trg) ) $ transitions aut
        pstate i = "q"++(show i)
        plab (s,r,Send,(l,pl)) = (show (envi M.! r))++" ! "++l++pl
        plab (s,r,Receive,(l,pl)) = (show (envi M.! s))++" ? "++l++pl



causalDependency :: Configuration -> Label -> Label -> Bool
causalDependency (ss,qq) a@(p, q, d, m) b@(p', q', d', m') =
  (subject a == subject b)
  ||
  (
    (p==p') && (q==q') && (d==Send)
    &&
    (L.null $ qq M.! (p,q))
  )


dependence :: Configuration -> Label -> [Label] -> Label -> Bool
dependence c x ys z =  helper x ys z                       
  where helper x [] z = causalDependency c x z
        helper x (y:ys) z = ((causalDependency c x y) && (helper y ys z))
                            ||
                            (helper  x ys z)



----------------


rmveQuotes :: String -> String            
rmveQuotes (x:xs) =
    if x == '\"' then rmveQuotes xs
    else x : (rmveQuotes xs)
rmveQuotes [] = ""  
         
printStateM :: State -> Participant -> String
printStateM s id = "q"++(rmveQuotes $ (show id)++(show s))


printLabelDot :: Label -> String
printLabelDot (s, r, Send, (msg,pl)) = s++r++"!"++msg++pl
printLabelDot (s, r, Receive, (msg,pl)) = s++r++"?"++msg++pl

printMachine :: Machine -> Participant -> String
printMachine m id =
  let sstates = L.foldr (++) ""
                (L.map (\x -> (printStateM x id)++" [label= \""++(printStateM x id)++"\" "
                              ++(
                                if (x==(sinit m))
                                then "shape = \"box\""
                                else ""
                              )
                              ++"];\n") $ states m)
      transi = L.foldr (++) "" ( 
        L.map (\(s, (a, t)) -> 
                (printStateM s id)
                ++ " -> "++
                (printStateM t id)
                ++
                " [label= \""++(printLabelDot a)++"\"];\n"
              )
        $ transitions m)
    in sstates ++ transi


printSystem :: System -> String
printSystem list = "digraph CFSMs { \n"++(helper $ M.toList list)++"} \n"
  where helper ((i,x):xs) =
          let header = "subgraph cluster_"++i
                       ++" {\n"
                       ++ "label = "++(show i)++";\n"      
              footer = "}\n"
              machine = printMachine x i     
          in (header++machine++footer)++"\n"++(helper xs)
        helper [] = ""
              


synchronise :: TS -> TS
synchronise aut = Automaton { states = nub $ concat $ L.map (\x -> [fst x, snd . snd $ x]) ntrans
                            , sinit = sinit aut
                            , transitions = nub $ ntrans
                            }
  where ntrans = helper [] (sinit aut)
        helper seen s 
          | s `L.elem` seen = []
          | otherwise = let current = [ (s,(x,s4)) -- [(s,(x,s2)), (s2,(y,s4))]
                                      | (x,s2) <- successors aut s
                                      , (y,s4) <- successors aut s2
                                      , dual x y, direction x == Send
                                      ]
                            next = nub $ L.map (\x -> (snd . snd $  x)) current
                        in (current)
                           ++
                           (concat $  L.map (helper (s:seen)) next)
