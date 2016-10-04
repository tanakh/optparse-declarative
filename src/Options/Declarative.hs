{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiWayIf                 #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

-- | Declarative options parser

module Options.Declarative (
    -- * Command type
    IsCmd,
    Cmd,
    logStr,
    getVerbosity,
    getLogger,

    -- * Argument definition tools
    Option(..),
    Flag,
    Arg,

    -- * Defining argment types
    ArgRead(..),
    Def,

    -- * Subcommands support
    Group(..),
    SubCmd, subCmd,

    -- * Run a command
    run, run_,
    ) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Reader
import           Control.Monad.Trans
import           Data.List
import           Data.Maybe
import           Data.Proxy
import           GHC.TypeLits
import           System.Console.GetOpt
import           System.Environment
import           System.Exit
import           System.IO
import           Text.Read

-- | Command line option
class Option a where
    -- | Type of the argument' value
    type Value a :: *
    -- | Get the argument' value
    get :: a -> Value a

-- | Named argument
newtype Flag (shortNames  :: Symbol  )
             (longNames   :: [Symbol])
             (placeholder :: Symbol  )
             (help        :: Symbol  )
             a
    = Flag { getFlag :: a }

-- | Unnamed argument
newtype Arg (placeholder :: Symbol) a = Arg { getArg :: a }

instance ArgRead a => Option (Flag _a _b _c _d a) where
    type Value (Flag _a _b _c _d a) = Unwrap a
    get = unwrap . getFlag

instance Option (Arg _a a) where
    type Value (Arg _a a) = a
    get = getArg

-- | Command line option's annotated types
class ArgRead a where
    -- | Type of the argument
    type Unwrap a :: *
    type Unwrap a = a

    -- | Get the argument's value
    unwrap :: a -> Unwrap a
    default unwrap :: a ~ Unwrap a => a -> Unwrap a
    unwrap = id

    -- | Argument parser
    argRead :: Maybe String -> Maybe a
    default argRead :: Read a => Maybe String -> Maybe a
    argRead s = readMaybe =<< s

    -- | Indicate this argument is mandatory
    needArg :: Proxy a -> Bool
    needArg _ = True

instance ArgRead Int

instance ArgRead Integer

instance ArgRead Double

instance ArgRead String where
    argRead = id

instance ArgRead Bool where
    argRead Nothing = Just False
    argRead (Just "f") = Just False
    argRead (Just "t") = Just True
    argRead _ = Nothing

    needArg _ = False

instance ArgRead a => ArgRead (Maybe a) where
    argRead Nothing = Just Nothing
    argRead (Just a) = Just <$> argRead (Just a)

-- | The argument which has defalut value
newtype Def (defaultValue :: Symbol) a =
    Def { getDef :: a }

instance (KnownSymbol defaultValue, ArgRead a) => ArgRead (Def defaultValue a) where
    type Unwrap (Def defaultValue a) = Unwrap a
    unwrap = unwrap . getDef

    argRead s =
        let s' = fromMaybe (symbolVal (Proxy :: Proxy defaultValue)) s
        in Def <$> argRead (Just s')

-- | Command
newtype Cmd (help :: Symbol) a =
    Cmd { unCmd :: ReaderT Int IO a }
    deriving (Functor, Applicative, Monad, MonadIO)

-- | Output string when the verbosity level is greater than or equal to `logLevel`
logStr :: Int         -- ^ Verbosity Level
       -> String      -- ^ Message
       -> Cmd help ()
logStr logLevel msg = do
    l <- getLogger
    l logLevel msg

-- | Return the verbosity level ('--verbosity=n')
getVerbosity :: Cmd help Int
getVerbosity = Cmd ask

-- | Retrieve the logger function
getLogger :: MonadIO m => Cmd a (Int -> String -> m ())
getLogger = do
    verbosity <- getVerbosity
    return $ \logLevel msg -> when (verbosity >= logLevel) $ liftIO $ putStrLn msg

-- | Command group
data Group =
    Group
    { groupHelp :: String
    , groupCmds :: [SubCmd]
    }

-- | Sub command
data SubCmd = forall c. IsCmd c => SubCmd String c

-- | Command class
class IsCmd c where
    getCmdHelp  :: c -> String
    default getCmdHelp :: (c ~ (a -> b), IsCmd b) => c -> String
    getCmdHelp f = getCmdHelp $ f undefined

    getOptDescr :: c -> [OptDescr (String, String)]
    default getOptDescr :: (c ~ (a -> b), IsCmd b) => c -> [OptDescr (String, String)]
    getOptDescr f = getOptDescr $ f undefined

    getUsageHeader :: c -> String -> String
    default getUsageHeader :: (c ~ (a -> b), IsCmd b) => c -> String -> String
    getUsageHeader f = getUsageHeader $ f undefined

    getUsageFooter :: c -> String -> String
    default getUsageFooter :: (c ~ (a -> b), IsCmd b) => c -> String -> String
    getUsageFooter f = getUsageFooter $ f undefined

    runCmd :: c
           -> [String]            -- ^ Command name
           -> Maybe String        -- ^ Version
           -> [(String, String)]  -- ^ Options
           -> [String]            -- ^ Non options
           -> [String]            -- ^ Unrecognized options
           -> IO ()

class KnownSymbols (s :: [Symbol]) where
    symbolVals :: Proxy s -> [String]

instance KnownSymbols '[] where
    symbolVals _ = []

instance (KnownSymbol s, KnownSymbols ss) => KnownSymbols (s ': ss) where
    symbolVals _ = symbolVal (Proxy :: Proxy s) : symbolVals (Proxy :: Proxy ss)

instance ( KnownSymbol shortNames
         , KnownSymbols longNames
         , KnownSymbol placeholder
         , KnownSymbol help
         , ArgRead a
         , IsCmd c )
         => IsCmd (Flag shortNames longNames placeholder help a -> c) where
    getOptDescr f =
        let flagname = head $
                       symbolVals (Proxy :: Proxy longNames) ++
                       [ [c] | c <- symbolVal (Proxy :: Proxy shortNames) ]
        in Option
            (symbolVal (Proxy :: Proxy shortNames))
            (symbolVals (Proxy :: Proxy longNames))
            (if needArg (Proxy :: Proxy a)
             then ReqArg
                  (flagname, )
                  (symbolVal (Proxy :: Proxy placeholder))
             else NoArg
                  (flagname, "t"))
            (symbolVal (Proxy :: Proxy help))
        : getOptDescr (f undefined)

    runCmd f name mbver options nonOptions unrecognized =
        let flagname = head $
                       symbolVals (Proxy :: Proxy longNames) ++
                       [ [c] | c <- symbolVal (Proxy :: Proxy shortNames) ]
            mbs = lookup flagname options
        in case (argRead mbs, mbs) of
            (Nothing, Nothing) ->
                errorExit name $ "flag must be specified: --" ++ flagname
            (Nothing, Just s) ->
                errorExit name $ "bad argument: --" ++ flagname ++ "=" ++ s
            (Just arg, _) ->
                runCmd (f $ Flag arg) name mbver options nonOptions unrecognized

instance ( KnownSymbol placeholder, IsCmd c )
         => IsCmd (Arg placeholder String -> c) where
    getUsageHeader f prog =
        " " ++ symbolVal (Proxy :: Proxy placeholder) ++ getUsageHeader (f undefined) prog

    runCmd f name mbver options nonOptions unrecognized =
        case nonOptions of
            [] -> errorExit name "not enough arguments"
            (opt: rest) ->
                case argRead (Just opt) of
                    Nothing ->
                        errorExit name $ "bad argument: " ++ opt
                    Just arg ->
                        runCmd (f $ Arg arg) name mbver options rest unrecognized

instance ( KnownSymbol placeholder, IsCmd c )
         => IsCmd (Arg placeholder [String] -> c) where
    getUsageHeader f prog =
        " " ++ symbolVal (Proxy :: Proxy placeholder) ++ getUsageHeader (f undefined) prog

    runCmd f name mbver options nonOptions unrecognized =
        runCmd (f $ Arg nonOptions) name mbver options [] unrecognized

instance KnownSymbol help => IsCmd (Cmd help ()) where
    getCmdHelp  _ = symbolVal (Proxy :: Proxy help)
    getOptDescr _ = []

    getUsageHeader _ _ = ""
    getUsageFooter _ _ = ""

    runCmd (Cmd m) name _ options nonOptions unrecognized =
        case (options, nonOptions, unrecognized) of
            (_, [], []) -> do
                let verbosityLevel = fromMaybe 0 $ do
                        s <- lookup "verbose" options
                        if | null s -> return 1
                           | all (== 'v') s -> return $ length s + 1
                           | otherwise -> readMaybe s
                runReaderT m verbosityLevel

            _ -> do
                forM_ nonOptions $ \o ->
                    errorExit name $ "unrecognized argument '" ++ o ++ "'"
                forM_ unrecognized $ \o ->
                    errorExit name $ "unrecognized option '" ++ o ++ "'"
                exitFailure

instance IsCmd Group where
    getCmdHelp = groupHelp
    getOptDescr _ = []

    getUsageHeader _ _ = " <COMMAND> [ARGS...]"
    getUsageFooter g _ = unlines $
        [ ""
        , "Commands: "
        ] ++
        [ "  " ++ name ++ replicate (12 - length name) ' ' ++ getCmdHelp c
        | SubCmd name c <- groupCmds g
        ]

    runCmd g name mbver _options (cmd: nonOptions) unrecognized =
        case [ SubCmd subname c | SubCmd subname c <- groupCmds g, subname == cmd ] of
           [SubCmd subname c] ->
               run' c (name ++ [subname]) mbver (nonOptions ++ unrecognized)
           _ ->
               errorExit name $ "unrecognized command: " ++ cmd
    runCmd _ name _ _ _ _ =
        errorExit name "no command given"

-- | Make a sub command
subCmd :: IsCmd c => String -> c -> SubCmd
subCmd = SubCmd

-- runner

run' :: IsCmd c => c -> [String] -> Maybe String -> [String] -> IO ()
run' cmd name mbver args = do
    let optDescr =
            getOptDescr cmd
            ++ [ Option "?" ["help"]    (NoArg ("help",    "t")) "display this help and exit" ]
            ++ [ Option "V" ["version"] (NoArg ("version", "t")) "output version information and exit"
               | isJust mbver ]
            ++ [ Option "v" ["verbose"] (OptArg (\arg -> ("verbose", fromMaybe "" arg)) "n") "set verbosity level" ]

        prog     = unwords name
        vermsg   = prog ++ maybe "" (" version " ++) mbver
        header = "Usage: " ++ prog ++ " [OPTION...]" ++ getUsageHeader cmd prog ++ "\n" ++
                 "  " ++ getCmdHelp cmd ++ "\n\n" ++
                 "Options:"

        usage    =
            usageInfo header optDescr ++
            getUsageFooter cmd prog

    case getOpt' RequireOrder optDescr args of
        (options, nonOptions, unrecognized, errors)
            | not $ null errors ->
                  errorExit name $ intercalate ", " errors
            | isJust (lookup "help" options) -> do
                  putStr usage
                  exitSuccess
            | isJust (lookup "version" options) -> do
                  putStrLn vermsg
                  exitSuccess
            | otherwise ->
                  runCmd cmd name mbver options nonOptions unrecognized

-- | Run a command with specifying program name and version
run :: IsCmd c => String -> Maybe String -> c -> IO ()
run progName progVer cmd =
    run' cmd [progName] progVer =<< getArgs

-- | Run a command
run_ :: IsCmd c => c -> IO ()
run_ cmd = do
    progName <- getProgName
    run progName Nothing cmd

errorExit :: [String] -> String -> IO ()
errorExit name msg = do
    let prog = unwords name
    hPutStrLn stderr $ prog ++ ": " ++ msg
    hPutStrLn stderr $ "Try '" ++ prog ++ " --help' for more information."
    exitFailure
