import Data.Maybe (listToMaybe)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x : xs) (y : ys) = f x y : zipWith' f xs ys

-- if the key matches key then return (k,v)

lookupAll :: Eq key => key -> [(key, value)] -> [value]
lookupAll key kvs = [v | (k, v) <- kvs, k == key]

-- make a defintion of the standard lookup function --
lookup' :: Eq key => key -> [(key, a)] -> Maybe a
lookup' key = listToMaybe . lookupAll key

-- if key is in (k,v) then replace v with val, else append the pair

update :: Eq key => key -> value -> [(key, value)] -> [(key, value)]
update k v [] = [(k, v)]
update k v ((oldk, oldv) : kvs)
  | oldk == k = (oldk, v) : kvs
  | otherwise = (oldk, oldv) : update k v kvs