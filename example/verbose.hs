{-# LANGUAGE DataKinds #-}

import           Options.Declarative

test :: Cmd "verbosity test" ()
test = do
    logStr 0 "verbosity level 0"
    logStr 1 "verbosity level 1"
    logStr 2 "verbosity level 2"
    logStr 3 "verbosity level 3"

main :: IO ()
main = run_ test
