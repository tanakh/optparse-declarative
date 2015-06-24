# optparse-declarative

`optparse-declarative` is a declarative and easy to use command-line option parser.

# Install

```bash
$ cabal install optparse-declarative
```

# Usage

## Writing a simple command

First, you need to enable `DataKinds` extension and import `Options.Declarative` module.

```hs
{-# LANGUAGE DataKinds #-}
import           Options.Declarative
```

Then, define the command line option as a **type of the function**.
For example, this is a simple greeting program:

```hs
greet :: Flag "g" '["greet"] "STRING" "greeting message" (Def "Hello" String)
      -> Arg "NAME" String
      -> Cmd "Greeting command" ()
greet msg name =
    liftIO $ putStrLn $ get msg ++ ", " ++ get name ++ "!"
```

There are two type of options, `Flag` and `Arg`.
`Flag` is named argument and `Arg` is unnamed argument.
Last argument of both options is value type.
If you need to specify default value, use the modifiers such as `Def`.

In above, variable `msg` has a very complex type (`Flag "g" '["greet"] "STRING" "greeting message" (Def "Hello" String)`).
In order to get the value of usual type (in this case, that is `String`),
you can use `get` function.

The whole type of command is `Cmd`.
`Cmd` is an instance of `MonadIO` and it has some extra information.

After defining a command, you just invoke it by `run_`.

```hs
main :: IO ()
main = run_ greet
```

You can execute this program like this:

```bash
$ ghc simple.hs

$ ./simple
simple: not enough arguments
Try 'simple --help' for more information.

$ ./simple --help
Usage: simple [OPTION...] NAME
Options:
  -g STRING  --greet=STRING  greeting message
  -?         --help          display this help and exit

$ ./simple World
Hello, World!

$ ./simple --greet=Goodbye World
Goodbye, World!
```

## Writing multiple sum-commands

You can write (nested) sub-commands.

Just groupe subcommands by `Group`, you got sub-command parser.

This is the example:

```hs
{-# LANGUAGE DataKinds #-}

import           Options.Declarative

main :: IO ()
main = run_ $
    Group "Test program for library"
    [ subCmd "greet"   greet
    , subCmd "connect" connect
    ]

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
```

And this is the output:

```bash
$ ./subcmd --help
Usage: subcmd [OPTION...] <COMMAND> [ARGS...]
Options:
  -?  --help  display this help and exit

Commands:
  greet       Greeting command
  port        Server command

$ ./subcmd connect --port=1234
connect to localhost:1234
```

For more examples, please see `example` directory.
