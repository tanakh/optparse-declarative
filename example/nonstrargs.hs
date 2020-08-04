{-# LANGUAGE DataKinds #-}

import           Control.Monad.Trans
import           Options.Declarative

sum' :: Arg "N" Int
     -> Arg "NS" [Int]
     -> Cmd "Simple greeting example" ()
sum' n ns =
    liftIO $ putStrLn $ show (get n) ++ ", " ++ show (sum $ get ns)

main :: IO ()
main = run_ sum'
