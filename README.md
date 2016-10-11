# optparse-declarative

`optparse-declarative` is a declarative and easy-to-use command-line option parser.

# Install

```bash
$ cabal install optparse-declarative
```

# Usage

## Writing a simple command

First, you need to enable `DataKinds` extension. Then import `Options.Declarative` module.

```hs
{-# LANGUAGE DataKinds #-}
import           Options.Declarative
```

Next, define command line options as a **type of the function**.
For example, this is a simple greeting program with `-g` option that
takes a message of type `String` and an unnamed command-line argument
that specifies a name:

```hs
greet :: Flag "g" '["greet"] "STRING" "greeting message" (Def "Hello" String)
      -> Arg "NAME" String
      -> Cmd "Greeting command" ()
greet msg name =
    liftIO $ putStrLn $ get msg ++ ", " ++ get name ++ "!"
```

There are two types of options, `Flag` and `Arg`.
`Flag` represents a named argument (e.g., `--greet "Hola"`), and `Arg` an unnamed argument (e.g., `John` of `greet --greet Hola John`).
The last argument of `Flag` and `Arg` is the type of the value of the
argument; in this example, they are both `String`.
You can specify any type for the value as long as the type is an
instance of `ArgRead` typeclass, in which the conversion function
from `String` to the specified type is defined.
`Options.Declarative` provides following instances of `ArgRead`
typeclass.

- Int
- Integer
- Bool
- Double
- String
- (ArgRead a) => Maybe a

Users can add a new instance of `ArgRead` to support any user-defined type.
Please see Section "How to add a new instance of `ArgRead`" for details.

If you wish to specify a default value for allowing users to omit a
value, use the modifier `Def` with the default value as the second type argument (and the third type argument is the type of the value).
You need to specify the default value in `String` instead of the final
value of the target type; the string will be converted to the final
value via `ArgRead` typeclass.

In the example above, the variable `msg` has a very complex type (`Flag "g" '["greet"] "STRING" "greeting message" (Def "Hello" String)`).
In order to get the value of the target type (in this case, that is `String`),
you can use `get` function.

The whole type of command is `Cmd`.
`Cmd` is an instance of `MonadIO` and it has some extra information.

Finally, you can run the whole program by `run_`.

```hs
main :: IO ()
main = run_ greet
```

Here is an example session with the program shown above.

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

Note that only the final option is used when multiple options of the
same name are given. This behavior emulates the behavior of a naive
program that uses GNU Getopt.

```bash
$ ./simple --greet=Hello --greet=Goodbye World
Goodbye, World!
```

There is another way of interpreting multiple options of the same name.
Suppose if you need to get multiple values from the same option.
Say, you wish to get `["Hello", "Goodbye"]` from the command-line
option `--greet=Hello --greeet=Goodbye`. Then, you can use
the modifier `List` to indicate that it accepts multiple values.
The first line of the function `greet` in the example above
would be changed as this:

```hs
greet :: Flag "g" '["greet"] "STRING" "greeting message" (List String)
```

The value returned by `get` will be a value of type `[String]`.
See the complete working example at `example/list.hs` for details.

The reason why we did not use `[String]` for specifying multiple
values was that it is hard to tell apart `String` from a list of
`Char` because `String` a type synonym of `[Char]`. If we only
allow other string types such as `Text` and drop the support for
`String`, we would be able to allow notations such as `[Text]` or
`[Int]`.


## Writing multiple subcommands

You can write (nested) subcommands.
You don't know what subcommands are? Imagine `git` command.
`git` has subcommands such as `git add`, `git commit`, `git log`, etc.
`git` has nested subcommands such as `git remote add`, `git remote rm`,
etc.
`optparse-declarative` provides an easy way to provide such possibly
nested subcommands.

Just group subcommands by `Group`, then you get a subcommand parser.
Here is an example with two subcommands `greet` and `connect`:

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

This is a sample session for the program above:

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

If you wish to specify the program name or the version number,
use `run` instead of `run_`. The first argument of `run` is
a program name (of type `String`). The second argument is
a version number (of type `Maybe String`).

```hs
main :: IO ()
main = run "program_name" (Just "1.3.2") $
    Group "Test program for library"
    [ subCmd "greet"   greet
    , subCmd "connect" connect
    ]
```

For more examples, please see `example` directory.


## Default options
`optparse-declarative` provides a few default options.
For example, `--help` is defined automatically so users do not have to
write it by their own. If run with `run` and the version number,
`--version` is defined automatically. Also, `--verbosity` option (`-v`
for short) is defined by default.
`getVerbosity` returns the verbosity level in `Int`.
`-v` gives 1, `-vv` gives 2, `-vvv` gives 3.
Alternatively, `--verbose=3` would yield 3.


## How to add a new instance of `ArgRead`
Users need to create an instance of `ArgRead` for supporting a new type
for the command line argument. Here is the definition of class
`ArgRead`.

```hs
class ArgRead a where
    -- | Type of the argument
    type Unwrap a :: *
    type Unwrap a = a

    -- | Get the argument's value
    unwrap :: a -> Unwrap a
    default unwrap :: a ~ Unwrap a => a -> Unwrap a
    unwrap = id

    -- | Argument parser
    argRead :: [String] -> Maybe a
    default argRead :: Read a => [String] -> Maybe a
    argRead ss = getLast $ mconcat $ Last . readMaybe <$> ss

    -- | Indicate this argument is mandatory
    needArg :: Proxy a -> Bool
    needArg _ = True
```

Suppose you are adding a support for your type `T`.
We explain which function to define explicitly, depending on the
property of `T`.

If `T` is the type of the final value you take out of a command line,
you do not have to define `Unwrap`. If `T` is a wrapper like `Def` or
`List`, define `type Unwrap T = <unwrapped type>`. For `Def x y`,
`type Unwrap (Def x y) = y`, while `type Unwrap (List x) = [x]` for
`List x`. If you defined `Unwrap`, define `unwrap` that takes
an actual value out of the wrapped value.

`argRead` is the main function that converts String into a value.
If the type is an instance of `Read` and you are satisfied with
how `read` converts a `String` into value, there is no need to
define your own `argRead`. Otherwise, you define a function that
converts a `String` into a value of the target type. When parsing
is successful, return `Just`. When it fails, return `Nothing`.
If the input is `[]`, it indicates the option does not have an
argument; otherwise the input is a list of a single `String`.
Last but not least, define `needArg _ = False` when the option
allows us to omit the associated value; consider a boolean
option like `--help`.
