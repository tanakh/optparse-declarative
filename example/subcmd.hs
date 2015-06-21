{-# LANGUAGE DataKinds #-}

import           Options.Declarative

greet :: Flag "g" '["greet"] "STRING" "greeting message" (Def "Hello" String)
      -> Flag "" '["decolate"] "" "decolate message" Bool
      -> Arg "NAME" String
      -> Cmd "Greeting command"
greet msg deco name = Cmd $ do
    let f x | get deco = "*** " ++ x ++ " ***"
            | otherwise = x
    putStrLn $ f $ get msg ++ ", " ++ get name ++ "!"

connect :: Flag "h" '["host"] "HOST" "host name"   (Def "localhost" String)
        -> Flag "p" '["port"] "PORT" "port number" (Def "8080"      Int   )
    -> Cmd "Connect command"
connect host port = Cmd $ do
    let addr = get host ++ ":" ++ show (get port)
    putStrLn $ "connect to " ++ addr

getOptExample
    :: Flag "o" '["output"] "FILE" "output FILE" (Def "stdout" String)
    -> Flag "c" '[] "FILE" "input FILE" (Def "stdin" String)
    -> Flag "L" '["libdir"] "DIR" "library directory" String
    -> Cmd "GetOpt example"
getOptExample output input libdir = Cmd $
    print (get output, get input, get libdir)

main :: IO ()
main = run_ $
    Group "Test program for library"
    [ subCmd "greet"   greet
    , subCmd "connect" connect
    , subCmd "getopt"  getOptExample
    ]
