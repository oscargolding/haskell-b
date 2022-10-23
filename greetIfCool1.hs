-- greetIfCool1.hs

module GreetIfCool1 where

greetifCool :: String -> String -> IO ()
greetifCool coolness second =
  if cool coolness
    then putStrLn "eyyyyy. What's shakin'?"
    else putStrLn "psshhhhh."
  where
    cool v = v == "downright frosty yo"

topFunc :: (Int, [a]) -> (Int, [a]) -> (Int, [a])
topFunc (a, b) (c, d) = (a + c, b ++ d)