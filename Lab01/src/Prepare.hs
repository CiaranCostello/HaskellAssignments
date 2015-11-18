module Main where
import Data.Char

{- extract dropsome, takesome, distance and handleInput, and bundle them -}


main 
 = do txt <- readFile "Main.hs"
      let lns = lines txt
      let funs = getWanted [] lns
      let bundle = wrapFuns funs
      writeFile "Submission.hs" $ unlines bundle
      
dropsome = "dropsome"
takesome = "takesome"
distance = "distance"
handleInput = "handleInput"
      
getWanted wanted [] = reverse wanted
getWanted wanted (ln:lns)
 | fstword == dropsome  = collect dropsome (ln:wanted) lns
 | fstword == takesome  = collect takesome (ln:wanted) lns
 | fstword == distance  = collect distance (ln:wanted) lns
 | fstword == handleInput  = collect handleInput (ln:wanted) lns
 | otherwise = getWanted wanted lns
 where fstword = hd ln
 
collect thing wanted [] = reverse wanted
collect thing wanted llns@(ln:lns) 
 | null fstword      =  collect thing (ln:wanted) lns
 | fstword == thing  =  collect thing (ln:wanted) lns
 | otherwise         =  getWanted wanted llns
 where fstword = hd ln 
 
hd = fst . span isAlpha 

wrapFuns funs = preamble ++ funs

preamble
 = [ "module Submission where"
   , "import Datatypes"
   , ""
   ]
