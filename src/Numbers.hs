module Numbers where

import Data.Functor.Foldable

toDigits :: Int -> [Int]
toDigits m = reverse $ ana coalg (Just m)
  where
    coalg :: (Maybe Int) -> ListF Int (Maybe Int)
    coalg (Just n)
      | n >= 10 = Cons (n `mod` 10) (Just (n `div` 10))
      | otherwise = Cons n Nothing
    coalg Nothing = Nil
