module GlobalGraph where

import Data.Set as S
import Data.List as L
import Data.Map as M
-- import Misc
import PetriNet

data Vertex = ParGate (Set Vertex) (Set Vertex)
            | OrGate (Set Vertex) (Set Vertex)
            | VT Transition
            | VP Place
            | VF Flow
            | Sink (Set Vertex)
            | Source
            deriving (Ord, Eq, Show)
                     
type Arc = (Vertex, Vertex)

type GlobalGraph = (Set Vertex, Set Arc)

emptyGG :: GlobalGraph
emptyGG = (S.empty, S.empty)


nodeNumber :: GlobalGraph -> Int
nodeNumber (vs, as) = S.size vs

transNumber :: GlobalGraph -> Int
transNumber (vs, as) = S.size as

isPort :: Vertex -> Bool
isPort x = case x of VF t -> True       
                     _ -> False

isPlace :: Vertex -> Bool
isPlace x = case x of VP t -> True
                      _ -> False
                      
isTransition :: Vertex -> Bool
isTransition x = case x of VT t -> True
                           _ -> False
                           
isSilent:: Vertex -> Bool
isSilent x = case x of VT (Silent t) -> True
                       _ -> False                           
                       
isGGNode :: Vertex -> Bool
isGGNode x = not (isPlace x || isSilent x)


isSink :: Vertex -> Bool
isSink t = case t of (Sink x) -> True
                     _ -> False

isSource :: Vertex -> Bool
isSource t = case t of Source -> True
                       _ -> False
                     
getSource :: GlobalGraph -> Vertex
getSource (vs, arcs) = case S.toList $ S.filter isSource vs of
  (x:xs) -> x
  [] -> Source -- error $ "\n Global Graph (nodes): "++(show vs)



succG :: GlobalGraph -> Vertex -> Set Vertex
succG (vs, arcs) v =  S.map (\(x,y) -> y) $ S.filter (\(x,y) -> x == v) arcs

precG :: GlobalGraph -> Vertex -> Set Vertex
precG (vs, arcs) v =  S.map (\(x,y) -> x) $ S.filter (\(x,y) -> y == v) arcs

--
-- TRANSFORMATION NUMBER THREE
--
net2globalgraph :: Net -> GlobalGraph
net2globalgraph pn@(pplaces, init, events, flow) = unionGG (transfPlace pplaces) (transfTrans events)
  where transfPlace ps = S.fold unionGG emptyGG $ S.map (\x -> unionGG (intoPlace pn x) (outofPlace pn x)) ps
        --
        transfTrans ts = S.fold unionGG emptyGG $ S.map (\x -> unionGG (intoTransition pn x) (outofTransition pn x)) ts


unionGG :: GlobalGraph -> GlobalGraph -> GlobalGraph
unionGG (vs1, acs1) (vs2, acs2) =  (nvs, S.union arcs addarcs) -- ((S.union vs1 vs2),(S.union acs1 acs2)) 
  where
    port1 = S.filter isPort vs1
    port2 = S.filter isPort vs2
    --
    nvs = S.difference (S.union vs1 vs2) (S.intersection port1 port2)
    arcs = S.intersection (S.union acs1 acs2) (cartProd nvs nvs)
    pairArcs1 = [ (v1,v2') | (v1,v1') <- (S.toList acs1), (v2,v2') <- (S.toList acs2), ((v1'==v2) && (isPort v2))]
    pairArcs2 = [ (v2,v1') | (v1,v1') <- (S.toList acs1), (v2,v2') <- (S.toList acs2), ((v2'==v1) && (isPort v1))]
    addarcs = S.union (S.fromList pairArcs1) (S.fromList pairArcs2) 



intoPlace :: Net -> Place -> GlobalGraph
intoPlace pn@(pplaces, init, events, flow) p = 
  let preset = preSet pn p
  in
   if (S.size preset) == 0
   then (S.insert Source (S.singleton (VP p)) , S.singleton (Source, (VP p)))
   else if (S.size preset) > 1
        then let fws = S.map (\t -> VF $ TP(t,p)) preset
                 gate = OrGate (fws) (S.singleton (VP p))
                 vs = S.insert gate $ S.insert (VP p) $ fws
                 acs = S.insert (gate, (VP p)) (S.map (\f -> (f,gate)) fws)
             in (vs, acs)
        else -- (S.size preset) == 1
          if (not $ S.member p init)
          then
            let t = head $ S.toList $ preset
            in (S.insert (VF $ TP(t,p)) (S.singleton (VP p)) , S.singleton (VF $ TP(t,p), VP p))
          else
            let t = head $ S.toList $ preset
            in (S.insert (VF $ TP(t,p)) (S.insert Source (S.singleton (VP p))) , 
                S.insert (Source, (VP p)) (S.singleton (VF $ TP(t,p), VP p)))
             


outofPlace :: Net -> Place -> GlobalGraph
outofPlace pn p =
  let post = postSet pn p
  in
   if (S.size post) > 1
   then let fws = S.map (\t -> VF $ PT(p,t)) post
            gate = OrGate (S.singleton (VP p)) (fws)
            vs = S.insert gate $ S.insert (VP p) $ fws
            acs = S.insert (VP p, gate) (S.map (\f -> (gate,f)) fws)
        in (vs, acs)
   else if (S.size post) == 1
        then let t = head $ S.toList $ post
             in ( S.insert (VF $ PT(p,t)) (S.singleton (VP p)) , S.singleton (VP p, VF $ PT(p,t)) )
        else (
          S.insert (Sink (S.singleton (VP p))) (S.singleton (VP p)) , 
          S.singleton (VP p, Sink (S.singleton (VP p))) 
          )
             
             
intoTransition :: Net -> Transition -> GlobalGraph
intoTransition pn t = 
  let preset = preSetTrans pn t
  in
   if (S.size preset) > 1
   then  let fws = S.map (\p -> VF $ PT(p,t)) preset
             gate = ParGate (fws) (S.singleton (VT t))
             vs = S.insert gate $ S.insert (VT t) $ fws
             acs = S.insert (gate, (VT t)) (S.map (\f -> (f,gate)) fws)
         in (vs, acs)
   else if (S.size preset) == 1
        then let p = head $ S.toList $ preset
             in (S.insert (VF $ PT(p,t)) (S.singleton (VT t)) , S.singleton (VF $ PT(p,t), VT t))
        else (S.insert Source (S.singleton (VT t)) , S.singleton (Source, (VT t)))
             
outofTransition :: Net -> Transition -> GlobalGraph
outofTransition pn t =
  let post = postSetTrans pn t
  in
   if (S.size post) > 1
   then let fws = S.map (\p -> VF $ TP(t,p)) post
            gate = ParGate (S.singleton (VT t)) (fws)
            vs = S.insert gate $ S.insert (VT t) $ fws
            acs = S.insert (VT t, gate) (S.map (\f -> (gate,f)) fws)
        in (vs, acs)
   else if (S.size post) == 1
        then let p = head $ S.toList $ post
             in ( S.insert (VF $ TP(t,p)) (S.singleton (VT t)) , S.singleton (VT t, VF $ TP(t,p)) )
        else (
          S.insert (Sink (S.singleton (VT t))) (S.singleton (VT t)) , 
          S.singleton (VT t, Sink (S.singleton (VT t))) 
          )             
             
             
             
--
-- TRANFORMATION NUMBER FOUR (CLEAN UP)
--
cleanupGG :: GlobalGraph -> GlobalGraph
cleanupGG = replaceNode
  -- let (v,a) =  replaceNode gg
  -- in (S.fold S.union S.empty $ S.map (\(x,y) -> S.insert x (S.singleton y)) a, a)
    
    
replaceNode :: GlobalGraph -> GlobalGraph
replaceNode gg@(vs, arcs) = 
  let replace = [ ( (x,p) , (p',x') ) |
                  (x,p) <- (S.toList arcs), (p',x') <- (S.toList arcs),
                  ((p==p') && not (isGGNode p) && ((S.size $ succG gg p) == 1 || (S.size $ precG gg p) == 1) )]
  in if (L.length replace) > 0
     then let ((x,p) , (p',x') ) = head replace
          in if (S.size $ succG gg p) == 1 && (S.size $ precG gg p) == 1
             then replaceNode (S.delete p vs, S.insert (x,x') (S.delete (p,x') (S.delete (x,p) arcs)) )
             else if (S.size $ precG gg p) == 1
                  then replaceNode (vs, S.insert (x,x') (S.delete (p,x) arcs) )
                  else -- (S.size $ succG p) == 1
                    replaceNode (vs, S.insert (x,x') (S.delete (x,p) arcs) )
     else gg  
--
-- PRINTING FUNCTIONS
--


ranks :: GlobalGraph -> Map Vertex Int
ranks gg = helper (getSource gg) 0 M.empty
  where helper init i map =  
          case M.lookup init map of 
            Nothing -> let list = S.toList (succG gg init)
                           newmap = M.insert init i map
                       in
                        dovertex list (i+1) newmap
            Just v -> map
        --
        dovertex (v:vs) i map = let newmap = helper v i map
                                in dovertex vs i newmap
        dovertex [] i map = map
        
ranksToVertices :: Map Vertex Int -> Map Int (Set Vertex)
ranksToVertices  inmap = helper (M.assocs inmap) M.empty
  where helper ((v,i):xs) outmap = case M.lookup i outmap of
          Just vertices -> helper xs (M.insert i (S.insert v vertices) outmap)
          Nothing -> helper xs (M.insert i (S.singleton v) outmap)
        helper [] outmap = outmap
        

             
printVertexLabel :: (Map String String) ->  Vertex -> String
printVertexLabel map (VT t) = "label=\""++(printTransition map t)++"\""
-- printVertexLabel _ = ""
printVertexLabel _ (VP p) = "label=\""++(printPlace p)++"\""
printVertexLabel _ f@(VF (PT(p,t))) = "label=\""++(printVertexId f)++"\""
printVertexLabel _ f@(VF (TP(t,p))) = "label=\""++(printVertexId f)++"\""
printVertexLabel _ (Sink vs) = "label=\""++""++"\""
printVertexLabel _ (ParGate vin vout) = "label=\""++"|"++"\""
printVertexLabel _ (OrGate vin vout) = "label=\""++"+"++"\""
printVertexLabel _ Source = "label=\""++"\""


sizeNode :: String
sizeNode = "0.3"

printVertexGraphics :: Vertex -> String
printVertexGraphics (VT (T t)) = "shape=box"
printVertexGraphics (Sink vs) = "shape=circle,width="++(sizeNode)++",height="++(sizeNode)++",fixedsize=true,peripheries=2"
printVertexGraphics (ParGate vin vout) = "shape=square,width="++(sizeNode)++",height="++(sizeNode)++",fixedsize=true"
printVertexGraphics (OrGate vin vout) = "shape=diamond,width="++(sizeNode)++",height="++(sizeNode)++",fixedsize=true"
printVertexGraphics Source = "shape=circle,width="++(sizeNode)++",height="++(sizeNode)++",fixedsize=true"
printVertexGraphics _ = ""


printVertexId :: Vertex -> String
printVertexId (VT t) = printTransitionId t
printVertexId (VP p) = printPlace p
printVertexId (VF (PT(p,t))) = "flow_"++(printPlace p)++"_"++(printTransitionId t)
printVertexId (VF (TP(t,p))) = "flow_"++(printTransitionId t)++"_"++(printPlace p)
printVertexId (Sink vs) = "sink_"++(S.fold (++) "" (S.map printVertexId vs))
printVertexId (Source) = "Gsource"
printVertexId (ParGate vin vout) = "par"
                                   ++
                                   (S.fold (++) "" (S.map printVertexId vin))
                                   ++
                                   (S.fold (++) "" (S.map printVertexId vout))
printVertexId (OrGate vin vout) = "or"
                                   ++
                                   (S.fold (++) "" (S.map printVertexId vin))
                                   ++
                                   (S.fold (++) "" (S.map printVertexId vout))

printRanks :: GlobalGraph -> String
printRanks gg = helper listranks ""
  where listranks = (M.assocs . ranksToVertices . ranks) gg -- [(i, {v1, ... vn}]
        helper ((i,vs):xs) string = 
          let
            nodeset = S.filter (\x -> not (isSource x || isSink x)) vs
            newline = if S.null nodeset then ""
                      else "{rank = "++(show i)++"; "++(
                        S.fold (++) "" (S.map (\x -> (printVertexId x)++"; ") nodeset) )++"}\n"
          in helper xs (string++newline)
        helper [] string = string


allSinks :: GlobalGraph -> Set Vertex
allSinks (vs , arcs) = S.filter isSink vs

globalGraph2String :: (Map String String) -> GlobalGraph -> String
globalGraph2String map gg@(vs , arcs) = 
  let header =  "digraph GG { \n"
      footer = "}\n"
      st_vertices = S.fold (++) ""
                    (S.map (\x -> (printVertexId x)++" ["++(printVertexLabel map x)++", "++(printVertexGraphics x)++"];\n") vs)
      st_arcs =  S.fold (++) "" (S.map (\(x,y) -> (printVertexId x)++" -> "++(printVertexId y)++"; \n") arcs)
      ranksinks = let sinks = allSinks gg in
        if S.null sinks
        then ""
        else "{rank = sink; "++( S.fold (++) "" (S.map (\x -> (printVertexId x)++"; ") sinks) )++"}\n"
  in header++
     st_vertices++
     "{rank = source; Gsource;}\n"++
     (printRanks gg)++
     ranksinks++
     st_arcs++
     footer
     

