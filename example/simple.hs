{-# LANGUAGE DataKinds #-}

import           Options.Declarative

greet :: Flag "g" '["greet"] "STRING" "greeting message" (Def "Hello" String)
      -> Arg "NAME" String
      -> Cmd "greet"
greet msg name = Cmd $ do
    putStrLn $ get msg ++ ", " ++ get name ++ "!"

main :: IO ()
main = run_ greet
