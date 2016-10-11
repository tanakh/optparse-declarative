{-# LANGUAGE DataKinds #-}

import           Control.Monad
import           Control.Monad.Trans
import           Options.Declarative

greet :: Flag "n" '["name"] "STRING" "name" (List String)
      -> Cmd "Count the number of people" ()
greet name =
    let people_name_list = get name
        num_people = length people_name_list
    in liftIO $ do
        putStrLn $ "There are " ++ show num_people ++ " people on the list."
        putStrLn " -- "
        forM_ people_name_list putStrLn

main :: IO ()
main = run_ greet
