{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

import           Control.Monad.Logger
import           Options.Declarative

test :: Cmd "verbosity test" ()
test = do
    $logDebug "this is debug level"
    $logInfo  "this is info level"
    $logWarn  "this is warn level"
    $logError "this is error level"

main :: IO ()
main = run_ test
