import Data.List
import Data.Char
import qualified Data.Map as Map  
import qualified Data.Set as Set
import Geometry
  
numUniques :: (Eq a) => [a] -> Int  
numUniques = length . nub

numUniquesT :: (Eq a) => [a] -> Int  
numUniquesT = \xs -> length (nub xs)

search :: (Eq a) => [a] -> [a] -> Bool  
search needle haystack =   
    let nlen = length needle  
    in  foldl (\acc x -> if take nlen x == needle then True else acc) False (tails haystack)
    
    
--same as map (chr . (+ 1) . ord) "Brian" 
encode :: Int -> String -> String  
encode shift msg = 
    let ords = map ord msg  
        shifted = map (+ shift) ords  
    in  map chr shifted
    
decode :: Int -> String -> String  
decode shift msg = encode (negate shift) msg


phoneBook =   
    [("betty","555-2938")  
    ,("bonnie","452-2928")  
    ,("patsy","493-2928")  
    ,("lucille","205-2928")  
    ,("wendy","939-8282")  
    ,("penny","853-2492")  
    ]
    
findKey :: (Eq k) => k -> [(k,v)] -> v  
findKey key xs = snd . head . filter (\(k,v) -> key == k) $ xs

findKey1 :: (Eq k) => k -> [(k,v)] -> Maybe v  
findKey1 key [] = Nothing  
findKey1 key ((k,v):xs) = if key == k  
                            then Just v  
                            else findKey1 key xs
                            
--implemented as a fold
findKey2 :: (Eq k) => k -> [(k,v)] -> Maybe v  
findKey2 key = foldr (\(k,v) acc -> if key == k then Just v else acc) Nothing



    