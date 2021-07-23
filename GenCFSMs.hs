-- C: rec x . {D!a; {D?a;x, D?b;x}, D!b; {D?a;x, D?b;x}}
import System.Environment
import Data.List



data Direction = Send 
               | Receive 
               deriving (Eq, Ord, Read, Show)
                        
type Label = (String, Direction, String)

type Transition = (String, (Label, String))

msglist = ['a'..'z']
participants = ['A'..'Z']




genTransitions :: Int -> String -> Direction -> [String] -> [[Transition]]
genTransitions _ _ _ [] = []
genTransitions _ _ _ [t] = []
genTransitions w p dir (s:t:xs) = ((branchings s t):(genTransitions w p dir (t:xs)))
  where genOneBranch s t i = (s, ((p, dir, [msglist!!i]), t))  
        branchings s t = map (genOneBranch s t) [0..w]


linkTogether :: Bool -> [[Transition]] -> [[Transition]] -> [Transition]
linkTogether _ [] _ = []
linkTogether _ _ [] = []
linkTogether flag xs ys = 
  let (sinit, _) = (head . head) xs
      (_,(_,mid)) = (head . last) xs
      nsndh = map (\(x,y) -> (mid,y)) $ head ys
      nsndt = map (\(x,(y,z)) -> (x,(y,sinit))) $ last ys
  in if flag 
     then concat $ xs++(nsndh:((init . tail) ys))++[nsndt]
     else concat $ xs++(nsndh:(tail ys))



genMachine :: Bool -> Int -> Int -> String -> [Transition]
genMachine flag x y p = 
  let sndstates = map (\x -> "qs"++(show x)) [0..y]
      rcvstates = map (\x -> "qr"++(show x)) [0..y]
  in linkTogether flag 
     (genTransitions x p Send sndstates)
     (genTransitions x p Receive rcvstates)
        


printMachine :: Bool -> Int -> Int -> String -> String
printMachine flag x y p = 
  ".outputs \n"
  ++".state graph\n"
  ++(helper xs)++"\n"
  ++ ".marking qs0\n"
  ++".end\n"
    where xs = genMachine flag x y p
          helper ys = intercalate "\n" $ map printTransition xs
          printTransition (s,((p,Send,m),t)) = s++" "++p++" ! "++m++" "++t
          printTransition (s,((p,Receive,m),t)) = s++" "++p++" ? "++m++" "++t
          
                  
mkSystem :: Bool -> Int -> Int ->  [String] -> String
mkSystem _ _ _ [] = ""
mkSystem flag x y (a:b:xs) = (printMachine flag x y b)++"\n"
                             ++(printMachine flag x y a)
                             ++"\n"++(mkSystem flag x y xs)

main :: IO ()
main = do args <- getArgs
          if ((length args) < 3)
            then do putStrLn "Usage: GenExample <breadth> <depth> <# of partition> [cycle]"
                    return ()
            else do let x = let sx = read (args!!0) :: Int in if (sx > 0) && (sx < 26) 
                                                              then sx-1
                                                              else 0
                        y = let sy = read (args!!1) :: Int in if (sy > 1)
                                                              then sy
                                                              else 2
                        p = 2*(read (args!!2) :: Int)
                        c = if (length args >3) 
                            then (read (args!!3) :: Int) > 0
                            else False
                    putStrLn $ "-- X:"++(show $ x+1)++" Y:"++(show y)++" P:"++(show p)++"\n"
                               ++(mkSystem c x y (map show [0..p-1]))
                    return ()