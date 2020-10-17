{-# LANGUAGE DataKinds #-}

import           Control.Monad.Trans
import           Options.Declarative

bflag :: Flag "b" '["boolflag"] "STRING" "boolean flag" Bool
      -> Cmd "Simple greeting example" ()
bflag b =
    liftIO $ putStrLn $ if get b then "Flag is True" else "Flag is False"

main :: IO ()
main = run_ bflag
