module Automata where

import Data.List as L
import Data.Map as M
import Data.Set as S
import Data.Function (on)
import Data.Maybe (catMaybes, isJust)

import qualified FiniteStateAutomata as FS
import qualified Hopcroft as H


-- DEBUG
import System.IO.Unsafe
import Debug.Trace

data Automaton a b = Automaton
                     { states :: [a]
                     , sinit :: a
                     , transitions :: [(a, (b, a))]
                     } deriving (Show, Eq, Ord)


outedges :: (Eq a, Eq b) => (Automaton a b) -> a -> [(b,a)]
outedges aut s = L.map snd $ L.filter (\(x,y) -> x==s) $ transitions aut


numberOfStates aut = length $ states aut
numberOfTransitions aut = length $ transitions aut


act2lab :: (Show b) => b -> String
act2lab = show 

printAutomaton :: (Ord a, Show a, Show b) => (b -> String) -> (Automaton a b) -> String
printAutomaton printlab aut =
  let envi = (M.fromList $ snd $  mapAccumL (\x y -> (x+1,(y,x))) 1 (states aut))
      sts = case M.lookup (sinit aut) envi  of
             Just it -> it:(L.delete (envi!(sinit aut)) (M.elems envi))
             Nothing -> error $ "Initial state not found -- is TS_k(S) empty!? "++(show aut)
      liststates = intercalate "\n" $ nub $ (L.map show sts)
      listtrans = intercalate "\n" $
                  L.map (\(x,(y,z)) -> (show $ envi!x)
                                     ++" "++(show $ envi!z)++" \""++(printlab y)++"\"") (transitions aut)
  in "n(0)\n---\n"++liststates++"\n---\n"++listtrans++"\n"

successors :: (Eq a) =>  Automaton a b -> a -> [(b,a)]
successors aut s = L.map snd $ L.filter (\x -> (fst x == s)) $ transitions aut

successor :: (Eq a, Eq b) =>  Automaton a b -> a -> b -> a
successor aut s l  = case L.map snd $ L.filter (\(x,(y,z)) -> (x == s) && (y == l)) $ transitions aut of
                      [] -> error "No succs"
                      ((x,y):xs) -> y



sortTransitionList :: (Eq a, Eq b, Ord b, Ord a) => [[(b,a)]] -> [[(b,a)]]
sortTransitionList xs = sortBy lorder xs
  where eorder (l,t) (l',t') = case compare l l' of
                                EQ -> compare t t'
                                t -> t
        lorder x y = compare (length y) (length x)          


reduce :: (Eq a, Eq b, Show a, Show b, Ord b, Ord a) => (b -> b -> Bool) -> Automaton a b -> Automaton a b
reduce f aut = let ntrans = helper [] [] (sinit aut)
               in Automaton { states = nub $ concat $ L.map (\x -> [fst x, snd . snd $ x]) ntrans
                            , sinit = sinit aut
                            , transitions = ntrans
                            }
  where helper todo seen s
          | s `L.elem` seen = []
          | otherwise = 
            case todo of
             [] -> case L.groupBy (\x y -> f (fst x) (fst y) ) $ successors aut s of
                    [] -> []
                    (h:xs) -> (L.map (\(x,y) -> (s,(x,y))) h)
                              ++
                              (concat $ L.map (\(x,y) -> helper xs (s:seen) y) h)
             (list:rest) -> (L.map (\(x,y) -> (s,(x, successor aut s x))) list)
                            ++
                            (concat $ L.map (\(x,y) -> helper rest (s:seen) (successor aut s x)) list)



integerAutomaton :: (Ord a) => Automaton a b -> Automaton Int b
integerAutomaton aut = Automaton { states = nub $ M.elems envi
                                 , sinit = envi M.! (sinit aut) 
                                 , transitions = L.map (\(x,(y,z)) -> (envi M.! x,(y, envi M.! z))) $ transitions aut
                                 }
  where envi = M.fromList $ snd $ mapAccumL (\x y -> (x+1, (y,x))) 0 (states aut)



stringAutomaton :: (Show a, Ord a) => Automaton a b -> Automaton String b
stringAutomaton aut = Automaton { states = L.map show $ states aut
                                , sinit = show (sinit aut) 
                                , transitions = L.map (\(x,(y,z)) -> (show x,(y, show z))) $ transitions aut
                                }

-- minimizeHop :: Automaton a (Maybe b) -> Automaton Int (Maybe b)
-- minimizeHop = translateFromHopcroft . H.hopcroft . translate2Hopcroft


translate2Hopcroft :: (Ord a, Ord b) => Automaton a b -> FS.DFA' b
translate2Hopcroft aut = FS.DFA' { FS.alpha = S.fromList $ L.map (fst .snd) $ transitions m
                                 , FS.ss = M.fromList $
                                            L.map (\s -> (s, mkStMap s)) $ states m
                                 , FS.accept = S.fromList $ states m
                                 , FS.st = sinit m
                               }
  where m = integerAutomaton aut
        mkStMap src = M.fromList $
                      L.map (\(x,(y,z)) -> (y, z)) $
                      L.filter (\(x,(y,z)) -> x == src) $ transitions m
        

translateFromHopcroft :: FS.DFA' b -> Automaton Int b
translateFromHopcroft nfa = Automaton { states = concat $ L.map (\(x,(y,z)) -> [x,z]) ntrans
                                      , sinit = FS.st nfa
                                      , transitions = ntrans
                                      }
  where ntrans = concat $
                 L.map flatten $ 
                 L.map (\(x,y) -> (x, M.toList y)) $ M.toList $ FS.ss nfa
        flatten (s, xs) = L.map (\(y,z) -> (s,(y,z))) xs


minimizeHop ::  (Ord a, Ord b, Show b) => Automaton a b ->  Automaton Int b
minimizeHop m = integerAutomaton . translateFromHopcroft . H.hopcroft . translate2Hopcroft $ m


dummyRemoveEpsilon :: (Eq a, Eq b) => Automaton a (Maybe b) -> Automaton [a] b
dummyRemoveEpsilon aut = Automaton { states = nub $ concat $ L.map (\(s,(l,t)) -> [s,t]) ntrans
                                   , sinit = [sinit aut]
                                   , transitions = ntrans
                                   }
  where ntrans = catMaybes $ L.map (\(x,(y,z)) -> case y of
                                                   Nothing -> Nothing
                                                   Just l -> Just ([x],(l,[z]))) $ transitions aut


concatSet :: (Ord a) => Set (Set a) -> Set a
concatSet s = S.foldr S.union S.empty s

removeEpsilon :: (Show a, Ord a, Ord b, Eq a, Eq b) => Automaton a (Maybe b) -> Automaton (Set a) b
removeEpsilon aut = Automaton { states = nub $ concat $ L.map (\(s,(l,t)) -> [s,t]) ntrans
                              , sinit = rinit
                              , transitions = ntrans
                              }
  where reach s seen
          | s `L.elem` seen = S.empty
          | otherwise = let sc = S.fromList $ L.filter (\x -> not $ isJust $ fst x) $ successors aut s
                        in S.insert s
                           (concatSet $ S.map (\x -> reach (snd x) (s:seen)) sc)
                           
        reaches xs = concatSet $ S.map (\x -> reachmap!x) xs
                       
        rinit = reachmap!(sinit aut)

        build seen s
          | s `L.elem` seen = []
          | otherwise = let sc =  L.map (\(Just l,t) -> (s,(l, reachmap!t))) $
                                  L.filter (\x -> isJust $ fst x) $
                                  S.toList $
                                  concatSet $ S.map (\x -> S.fromList $ successors aut x) (reachesmap!s)
                        in sc:(concat $ L.map (\x -> build (s:seen) $ snd . snd $ x) sc)
                           
        ntrans = concat $ build [] rinit
        
        
        reachmap = M.fromList $ L.map (\x -> (x, reach x [])) $ states aut
        reachesmap = M.fromList $
                     L.map (\x -> (x, reaches x)) $
                     L.map (\x -> reachmap!x) $ states aut
       

-- powerset [] = [[]]
-- powerset [x] = [[],[x]]
-- powerset xs = [] : runit (tail rsxtails) ps0
--    where
--         xstails = tails xs
--         rsxtails = reverse xstails
--         ps0 = L.map (const [[]]) xstails
--         psn tn psn1 = psnew
--            where
--             psnew = [tn]:
--              (zipWith (++)
--               (reverse (zipWith (\x psn1i -> L.map (x:) psn1i) xs (tail $ reverse$tail $ psn1)))
--               psnew)

--         runit [tn] _ = [xs]
--         runit (tn:tns) psn1 = (last newps) ++ (runit tns newps)
--             where newps = psn tn psn1
                  


determinise :: (Ord a, Ord b, Eq a, Eq b) => Automaton a b -> Automaton (Set a) b
determinise aut = Automaton { states = nub $ (S.singleton (sinit aut)):(concat $ L.map (\(s,(l,t)) -> [s,t]) ntrans)
                            , sinit = S.singleton (sinit aut)
                            , transitions = nub ntrans
                            }
  where ntrans = build (S.singleton (sinit aut)) []
        build ss seen
          | ss `L.elem` seen = []
          | otherwise = let next = concat $ S.toList $ S.map (successors aut) ss
                            sc a = S.fromList $
                                   L.map (snd . snd) $
                                   L.filter (\(s,(l,t)) -> a==l && s `L.elem` ss) $ transitions aut
                            trs = nub $ L.map (\a -> (ss,(a, sc a))) (L.map fst next)
                        in trs
                           ++
                           (concat $ L.map (\(s,(l,t)) -> build t (ss:seen)) trs)


findLabel :: (Eq a, Eq b) => Automaton a b -> a -> (b -> Bool) -> (b -> Bool) -> Bool
findLabel aut s fun gun = helper [] [s]
  where helper seen [] = False 
        helper seen (s:xs)
          | s `L.elem` seen = helper seen xs
          | otherwise = let sc = successors aut s
                        in (L.or $ L.map (fun . fst) sc)
                           ||
                           (helper (s:seen) (xs++(L.map snd $ L.filter (\x -> gun (fst x)) sc)))


findPathToState  :: (Eq a, Eq b) => Automaton a b -> a -> Maybe [b]
findPathToState aut s = case helper [] [] (sinit aut) of
                          Nothing -> Nothing
                          Just ls -> Just $ L.nub ls
  where helper path seen x 
          | x == s = Just path
          | x `L.elem` seen = Nothing
          | otherwise = let sc = successors aut x
                          in case catMaybes (L.map (\(l,s) -> helper (path++[l]) (x:seen) s) sc) of
                               [] -> Nothing
                               (p:ps) -> Just (head $ sortBy (compare `on` length) (p:ps))


        
mfindLabel :: (Eq a, Eq b) => Automaton a b -> a -> (b -> Bool) -> (b -> Bool) -> Maybe [a]
mfindLabel aut s fun gun = helper [] [s]
  where helper seen [] = Nothing
        helper seen (s:xs)
          | s `L.elem` seen = helper seen xs
          | otherwise = let sc = successors aut s
                        in if (L.or $ L.map (fun . fst) sc)
                           then Just seen
                           else (helper (s:seen) (xs++(L.map snd $ L.filter (\x -> gun (fst x)) sc)))


findDependence :: (Show b, Show a, Eq a, Eq b) => Automaton a b -> a -> b -> (a -> b -> [b] -> b -> Bool) -> (b -> Bool) -> Bool
findDependence aut s l depend bad = helper [] [] s
  where helper seen path s
          | s `L.elem` seen = True
          | otherwise = let sc = successors aut s
                            bds = L.filter (bad . fst) sc
                        in (L.and $ L.map (\x -> depend s l path (fst x)) bds)
                           &&
                           (L.and $ L.map (\z -> helper (s:seen) (path++[fst z]) (snd z)) sc)
                         
