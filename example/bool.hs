{-# LANGUAGE DataKinds #-}

import           Control.Monad.Trans
import           Options.Declarative

main' :: Flag "b" '["bool"] "STRING" "boolean flag" Bool
      -> Cmd "Simple greeting example" ()
main' b =
    liftIO $ putStrLn $ if get b then "Flag is True" else "Flag is False"

main :: IO ()
main = run_ main'
