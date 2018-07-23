sequenceA :: (Applicative f) => [f a] -> f [a]
sequenceA [] = pure []
sequenceA (x:xs) = (:) <$> x <*> sequenceA xs

sequenceAviaFoldR :: (Applicative f) => [f a] -> f [a]  
sequenceAviaFoldR = foldr (liftA2 (:)) (pure [])