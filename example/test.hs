{-# LANGUAGE DataKinds #-}

import Options.Declarative

foo :: Flag "g" '["greet"] "STRING" "greeting message" (Def "Hello" String)
    -> Flag "" '["decolate"] "" "decolate message" Bool
    -> Arg "NAME" String
    -> Cmd "command test 1"
foo greet deco name = Cmd $ do
    let f x | get deco = "*** " ++ x ++ " ***"
            | otherwise = x
    putStrLn $ f $ get greet ++ ", " ++ get name ++ "!"

bar :: Flag "h" '["host"] "HOST" "host name"   (Def "localhost" String)
    -> Flag "p" '["port"] "PORT" "port number" (Def "8080"      Int   )
    -> Cmd "command test 2"
bar host port = Cmd $ do
    let addr = get host ++ ":" ++ show (get port)
    putStrLn $ "connect to " ++ addr

getOptExample
    :: Flag "o" '["output"] "FILE" "output FILE" (Def "stdout" String)
    -> Flag "c" '[] "FILE" "input FILE" (Def "stdin" String)
    -> Flag "L" '["libdir"] "DIR" "library directory" String
    -> Cmd "GetOpt example"
getOptExample output input libdir = Cmd $ do
    print (get output, get input, get libdir)
    return ()

main :: IO ()
main = run_ $
    Group "Test program for library"
    [ subCmd "hello" foo
    , subCmd "port"  bar
    , subCmd "getopt" getOptExample
    ]

-- main :: IO ()
-- main = run_ $
--     Group "Cabal"
--     [ subCmd "update" foo
--     , subCmd "install"  bar
--     , subCmd "sandbox" $
--       Group "Sandbox instructions"
--       [ subCmd "add" foo
--       , subCmd "init" bar
--       ]
--     ]
