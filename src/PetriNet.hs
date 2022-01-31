module PetriNet where

import Data.Set as S
import Data.List as L
import Data.Map as M
-- import Misc
import Data.String.Utils as SU

data Place = P String
           deriving (Ord, Eq, Show)

data Transition = T String | Silent String
                deriving (Ord, Eq, Show)

data Flow = PT (Place, Transition) 
          | TP (Transition, Place)
          deriving (Ord, Eq, Show)
                     
            
type Net = (Set Place, Set Place, Set Transition, Set Flow)


--
-- TRANSFORMATION NUMBER ONE (One-source NET)
--
oneSource :: Net -> Net
oneSource pn@(pplaces, init, events, flow) =
  -- if (S.size init ) == 1 then pn
  -- else
    let np = P ("p_init")
        nt = Silent ("t_init")
        addflow = S.insert (PT (np,nt)) (S.map (\x -> TP (nt, x)) init)
    in 
     (S.insert np pplaces, S.singleton np , S.insert nt events, S.union addflow flow)

isPT :: Flow -> Bool
isPT (PT _) = True
isPT (TP _) = False

isTP :: Flow -> Bool
isTP p = not $ isPT p



--
-- TRANSFORMATION NUMBER TWO (Joined NET)
--
preSet :: Net -> Place -> Set Transition
preSet (pplaces, init, events, flow) p =
  S.map (\(TP (t,p')) -> t) (S.filter (\(TP (t,p')) -> p == p')  (S.filter (\f -> isTP f) flow))
  
  
postSet :: Net -> Place -> Set Transition
postSet (pplaces, init, events, flow) p =
  S.map (\(PT (p',t)) -> t) (S.filter (\(PT (p',t)) -> p == p')  (S.filter (\f -> isPT f) flow))
  
  
preSetTrans :: Net -> Transition -> Set Place
preSetTrans (pplaces, init, events, flow) t =
  S.map (\(PT (p,t')) -> p) (S.filter (\(PT (p,t')) -> t == t')  (S.filter (\f -> isPT f) flow))
  
  
postSetTrans :: Net -> Transition -> Set Place
postSetTrans (pplaces, init, events, flow) t =
  S.map (\(TP (t',p)) -> p) (S.filter (\(TP (t',p)) -> t == t')  (S.filter (\f -> isTP f) flow))
  
  
  
prepostTable :: Net -> (Place -> Set Transition) -> Map (Set Transition) (Set Place)
prepostTable pn@(pplaces, init, events, flow) f = helper M.empty (S.toList pplaces)
  where helper map (p:ps) =
          let 
            preset = f p
          in
          case M.lookup preset map of
            Just pset -> helper (M.insert preset (S.insert p pset) map) ps
            Nothing -> helper (M.insert preset (S.singleton p) map) ps
        helper map [] = map

presetTable :: Net -> Map (Set Transition) (Set Place)
presetTable pn = prepostTable pn (preSet pn)

postsetTable :: Net -> Map (Set Transition) (Set Place)
postsetTable pn = prepostTable pn (postSet pn)


joinPred :: Net -> Net
joinPred pn@(pplaces, init, events, flow) = helper 0 (M.toAscList (presetTable pn)) pn
  where helper i ((k,ps):mps) net@(pplaces, init, events, flow) = 
          if (S.size k) < 2 || (S.size ps) < 2
          then helper (i+1) mps net
          else 
            let np = P ("PJPRE"++(show i))
                nt = Silent ("TJPRE"++(show i))
                toadd = S.insert (PT (np,nt)) 
                        $ S.union  (S.map (\x -> TP(x,np)) k) (S.map (\x -> TP(nt,x)) ps)
                toremove = S.map (\(x,y) -> TP(x,y)) 
                           (S.fold S.union S.empty (S.map (\x -> cartProd k (S.singleton x)) ps))
                npn = (S.insert np pplaces, init, S.insert nt events, S.difference (S.union flow toadd) (toremove))
            in helper (i+1) mps npn
        helper _ [] net = net


joinPost :: Net -> Net
joinPost pn@(pplaces, init, events, flow) = helper 0 (M.toAscList (postsetTable pn)) pn
  where helper i ((k,ps):mps) net@(pplaces, init, events, flow) = 
          if (S.size k) < 2  || (S.size ps) < 2
          then helper (i+1) mps net
          else 
            let np = P ("PJPOST"++(show i))
                nt = Silent ("TJPOST"++(show i))
                toadd = S.insert (TP (nt,np)) 
                        $ S.union  (S.map (\x -> PT(np,x)) k) (S.map (\x -> PT(x,nt)) ps)
                toremove = S.map (\(x,y) -> PT(x,y)) 
                           (S.fold S.union S.empty (S.map (\x -> cartProd (S.singleton x) k) ps))
                npn = (S.insert np pplaces, init, S.insert nt events, S.difference (S.union flow toadd) (toremove))
            in helper (i+1) mps npn
        helper _ [] net = net


printTables :: Net -> IO()
printTables pn = do putStrLn "TABLES:"
                    putStrLn (show $ presetTable pn)
                    putStrLn (show $ postsetTable pn)

--
-- Parsing Functions
--
parsePetriNet :: [[String]] -> Net
parsePetriNet content = helper (S.empty, S.empty, S.empty, S.empty) content
  where
    helper pn (y:ys) = let pn' = helperLine pn y
                       in helper pn' ys
    helper pn [] = pn
    helperLine pn@(pplaces , init, transs, flows) (x:xs) =
          case x of 
            ".outputs" -> let  transs' = L.foldr (S.insert) S.empty (L.map (\t -> (T t)) xs)
                          in (pplaces, init, transs',  flows)
            ".state" -> pn
            ".marking" -> (pplaces, parseMarkedPlaces xs, transs,  flows)
            "#" -> pn
            ".model" -> pn
            ".end" -> let new_trans = getTransitions flows
                      in (nplaces, init, new_trans,  flows)
              where nplaces = S.map getPlace flows
            y -> let nflow = parseFlow transs y xs
                 in (pplaces, init, transs, S.union nflow flows)
    helperLine pn [] = pn


getTransitions :: Set Flow -> Set Transition
getTransitions flow = helper S.empty (S.toList flow)
  where helper acc ((PT(p,t)):xs) = helper (S.insert t acc) xs
        helper acc ((TP(t,p)):xs) = helper (S.insert t acc) xs
        helper acc [] = acc

getPlace :: Flow -> Place
getPlace (PT (p,t)) = p
getPlace (TP (t,p)) = p

-- INPUT: "{", "p5", "p6",.... "}"
parseMarkedPlaces :: [String] -> Set Place
parseMarkedPlaces (x:xs) =
  case x of "{" -> parseMarkedPlaces xs
            "}" -> S.empty
            s -> S.insert (P s) (parseMarkedPlaces xs)
parseMarkedPlaces [] = S.empty

isTrans :: String -> Set Transition -> Bool
isTrans str transs = S.member (T (removeTranSplit str)) transs

removeTranSplit :: String -> String
removeTranSplit s = let sp = SU.split "/" s in
  if (L.length sp) == 2
  then (sp!!0)
  else s


-- "p5", "alice0,bob0,0->1:bwin", "alice0,carol0,0->2:cwin" ---> { (p5,alice0,bob0,0->1:bwin) , (p5,alice0,carol0,0->2:cwin) }
parseFlow :: Set Transition -> String -> [String] -> Set Flow
parseFlow transs src xs = S.fromList (L.map (\x -> consFlow src x) xs)
  where consFlow src x = if isTrans x transs 
                         then PT (P src, T x) 
                         else TP (T src, P x)
                              
                              
                              
--
-- Printing Functions
--          
printPlace :: Place -> String
printPlace (P s) = "P"++s

printTransition :: (Map String String) -> Transition -> String
printTransition map t =  case labelTrans map t of
  Just s -> s
  Nothing -> "silent"

printTransitionId :: Transition -> String
printTransitionId (T t) = "T"++((SU.replace "/" "") . (SU.replace ":" "") . (SU.replace "->" "") .(SU.replace "," "")) t
printTransitionId (Silent t) = "T"++t

--
--    /!\  HERE INPUT EVENT TABLES 
--

labelTrans :: (Map String String) -> Transition -> Maybe String
labelTrans m (T t) = M.lookup (removeTranSplit t) m
  -- Just t  --
  -- let sp = SU.split "," t in
  --   if (L.length sp) == 3 
  --   then Just (sp!!2)
  --   else Nothing
labelTrans _ (Silent t) = Nothing
                    

printFlow :: Flow -> String
printFlow (PT (p,t)) = (printPlace p)++" -> "++(printTransitionId t)++"; \n"
printFlow (TP (t,p)) = (printTransitionId t)++" -> "++(printPlace p)++"; \n"

printNet :: (Map String String) -> Net -> String
printNet map (pplaces , init, transs, flows) =
  let header =  "digraph PN { \n"
      footer = "}\n"
      node_places = S.fold (++) "" $
                    S.map (\x -> (printPlace x)++" [label=\""++(printPlace x)++"\"]; \n ") pplaces
      node_trans = S.fold (++) "" $
                   S.map (\x -> (printTransitionId x)++" [label=\""++(printTransition map x)++"\"]; \n ") transs
      st_flow =  S.fold (++) "" (S.map printFlow flows)
  in
   header++node_places++node_trans++st_flow++footer
                



cartProd :: (Ord a, Ord b) => Set a -> Set b -> Set (a,b)
cartProd sa sb = S.fromList $ helper (S.toList sa) (S.toList sb)
  where helper xs ys = [(x,y) | x <- xs, y <- ys]
