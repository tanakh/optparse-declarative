{-# LANGUAGE DataKinds #-}

import           Control.Monad.Trans
import           Options.Declarative

greet :: Flag "g" '["greet"] "STRING" "greeting message" (Def "Hello" String)
      -> Flag "" '["decolate"] "" "decolate message" Bool
      -> Arg "NAME" String
      -> Cmd "Greeting command" ()
greet msg deco name = do
    let f x | get deco = "*** " ++ x ++ " ***"
            | otherwise = x
    liftIO $ putStrLn $ f $ get msg ++ ", " ++ get name ++ "!"

connect :: Flag "h" '["host"] "HOST" "host name"   (Def "localhost" String)
        -> Flag "p" '["port"] "PORT" "port number" (Def "8080"      Int   )
        -> Cmd "Connect command" ()
connect host port = do
    let addr = get host ++ ":" ++ show (get port)
    liftIO $ putStrLn $ "connect to " ++ addr

getOptExample
    :: Flag "o" '["output"] "FILE" "output FILE" (Def "stdout" String)
    -> Flag "c" '[] "FILE" "input FILE" (Def "stdin" String)
    -> Flag "L" '["libdir"] "DIR" "library directory" String
    -> Cmd "GetOpt example" ()
getOptExample output input libdir =
    liftIO $ print (get output, get input, get libdir)

main :: IO ()
main = run_ $
    Group "Test program for sub commands"
    [ subCmd "greet"   greet
    , subCmd "connect" connect
    , subCmd "getopt"  getOptExample
    ]
