-- -*- Mode: HASKELL; -*-

-- Main module of MIDA provides functions for interaction with user.

-- Copyright (c) 2014 Mark Karpov

-- This program is free software: you can redistribute it and/or
-- modify it under the terms of the GNU General Public License as
-- published by the Free Software Foundation, either version 3 of the
-- License, or (at your option) any later version.

-- This program is distributed in the hope that it will be useful, but
-- WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
-- General Public License for more details.

module Main (main) where

import Control.Monad.State.Strict
import qualified Data.Map as M
import System.Random.Mersenne.Pure64
import Options.Applicative
import System.FilePath (takeFileName, replaceExtension, combine)
import System.IO
import Text.Printf (printf)
import Codec.Midi (exportFile)
import Data.Char (isSpace)
import Data.List
import Control.Exception
import System.Directory (doesFileExist, getHomeDirectory)
import qualified System.Console.Haskeline as L
import Parser
import Environment
import Eval
import Translator
import Config

-- constants --

version      = "0.3.0"
cmdChar      = ':'
dfltDefs     = M.empty
dfltRandGen  = pureMT 0
dfltPrompt   = "mida> "
dfltPrvLen   = 16
dfltBlSize   = 4096
dfltFileName = "interactive"
dfltSeed     = 0
dfltQuarter  = 24
dfltBeats    = 16

-- command line processing --

data Opts = Opts
    { getIntr :: Bool
    , getSeed :: Int
    , getQ    :: Int
    , getBars :: Int
    , getOut  :: String
    , getFile :: String }

opts :: ParserInfo Opts
opts =  info (helper <*> bar)
      ( fullDesc
     <> progDesc "starts MIDA interpreter or translates source into MIDI file"
     <> header "mida - interpreter for MIDA language" )
    where bar =  Opts
             <$> switch
               ( long    "interactive"
              <> short   'i'
              <> help    "Enable interactive session" )
             <*> option  auto
               ( long    "seed"
              <> short   's'
              <> metavar "SEED"
              <> value   dfltSeed
              <> help    "Set seed for MIDI generation, default is 0" )
             <*> option  auto
               ( long    "quarter"
              <> short   'q'
              <> metavar "TICKS"
              <> value   dfltQuarter
              <> help    "Set ticks per quarter note, default is 24" )
             <*> option  auto
               ( long    "beats"
              <> short   'b'
              <> metavar "BEATS"
              <> value   dfltBeats
              <> help    "Set total time in quarter notes, default is 16" )
             <*> strOption
               ( long    "output"
              <> short   'o'
              <> metavar "OUT"
              <> value   ""
              <> help    "Specify non-standard output file name" )
             <*> argument str
               ( metavar "FILE"
              <> value   "" )

-- interactive environment --

trim :: String -> String
trim = f . f
    where f = reverse . dropWhile isSpace

aCmd :: String -> Bool
aCmd = isPrefixOf [cmdChar] . trim

isCmd :: String -> String -> Bool
isCmd x = (isPrefixOf $ cmdChar : x) . trim

commands = [ ("help",    cmdHelp,    "Show this help text")
           , ("license", cmdLicense, "Show license")
           , ("save",    cmdSave,    "Save current environment in file")
           , ("purge",   cmdPurge,   "Remove redundant definitions")
           , ("make",    cmdMake,    "Generate and save MIDI file")
           , ("def",     cmdDef,     "Print definition of given symbol")
           , ("prompt",  cmdPrompt,  "Set MIDA prompt")
           , ("length",  cmdLength,  "Set length of displayed results")
           , ("block",   cmdBlSize,  "Set size of block")
           , ("quit",    undefined,  "Quit the interactive environment") ]

printExc :: SomeException -> IO ()
printExc e = hPutStr stderr $ printf "-> %s;\n" (show e)

cmdHelp :: String -> StateT Env IO ()
cmdHelp _ = liftIO (printf "Available commands:\n") >> mapM_ f commands
    where f (cmd, _, text) = liftIO $ printf "  %c%-24s%s\n" cmdChar cmd text

cmdLicense :: String -> StateT Env IO ()
cmdLicense _ = liftIO $ printf
    "MIDA - realization of MIDA, language for generation of MIDI files.\n\
    \Copyright (c) 2014 Mark Karpov\n\
    \\n\
    \This program is free software: you can redistribute it and/or modify\n\
    \it under the terms of the GNU General Public License as published by\n\
    \the Free Software Foundation, either version 3 of the License, or\n\
    \(at your option) any later version.\n\
    \\n\
    \This program is distributed in the hope that it will be useful,\n\
    \but WITHOUT ANY WARRANTY; without even the implied warranty of\n\
    \MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the\n\
    \GNU General Public License for more details.\n\
    \\n\
    \You should have received a copy of the GNU General Public License\n\
    \along with this program.  If not, see <http://www.gnu.org/licenses/>.\n"

cmdSave :: String -> StateT Env IO ()
cmdSave given =
    do actual <- getFileName
       let file = if null given then actual else given
       source <- fullSrc
       setFileName file
       liftIO $ catch (do writeFile file source
                          printf "-> environment saved as \"%s\".\n" file)
                      printExc

cmdPurge :: String -> StateT Env IO ()
cmdPurge _ =
    do purgeEnv topDefs
       liftIO $ printf "-> environment purged;\n"

cmdMake :: String -> StateT Env IO ()
cmdMake arg =
    do file <- getFileName
       saveMidi (parseInt s dfltSeed)
                (parseInt q dfltQuarter)
                (parseInt b dfltBeats)
                (output f file)
    where (s:q:b:f:_) = (words arg) ++ repeat ""

cmdDef :: String -> StateT Env IO ()
cmdDef name =
    do def <- getSrc name
       liftIO $ putStr . unlines . map ("=> " ++) . lines $ def

cmdPrompt :: String -> StateT Env IO ()
cmdPrompt x = setPrompt (x ++ " ")

cmdLength :: String -> StateT Env IO ()
cmdLength x =
    do old <- getPrvLength
       setPrvLength $ parseInt x old

cmdBlSize :: String -> StateT Env IO ()
cmdBlSize x =
    do old <- getBlockSize
       setBlockSize $ parseInt x old

processCmd :: String -> StateT Env IO ()
processCmd input =
    case find f commands of
      (Just (_, x, _)) -> x args
      Nothing  -> liftIO $ printf "-> unknown command, try %chelp;\n" cmdChar
    where f (x, _, _)    = x == cmd
          (cmd', args')  = break isSpace input
          cmd            = filter (/= cmdChar) cmd'
          args           = trim args'

prettyList :: [Int] -> String
prettyList [] = "=> none"
prettyList xs = printf "=> %s..." $ intercalate " " (map show xs)

processExpr :: String -> StateT Env IO ()
processExpr expr =
    do file    <- getFileName
       case parseMida file expr of
         (Right x) -> mapM_ f x
         (Left  x) -> liftIO $ printf "parse error in %s\n" x
       where f (Definition n e s) =
                 do addDef n e s
                    liftIO $ printf "-> defined '%s'\n" n
             f (Exposition e) =
                 do l <- getPrvLength
                    r <- eval e l
                    liftIO . putStrLn . prettyList $ take l r

unfin :: String -> Bool
unfin arg = or [isSuffixOf "," s, f "[]", f "{}", f "<>", f "()"]
    where s = trim arg
          f [x,y] = ((&&) <$> (> 0) <*> (/= g y)) (g x)
          g x     = length $ filter (== x) s

getMultiline :: String -> L.InputT (StateT Env IO) (Maybe String)
getMultiline prv =
    do prompt <- lift getPrompt
       input  <- L.getInputLine $ if null prv
                                  then prompt
                                  else replicate (length prompt) ' '
       case (prv ++) . (++ "\n") <$> input of
         (Just x) -> if unfin x
                     then getMultiline x
                     else return (Just x)
         Nothing  -> return Nothing

interaction :: L.InputT (StateT Env IO) ()
interaction =
    do input <- getMultiline ""
       case input of
         (Just x) -> if isCmd "quit" x
                     then return ()
                     else do if aCmd x
                             then lift $ processCmd x
                             else lift $ processExpr x
                             interaction
         Nothing    -> return ()

loadConfig :: String -> StateT Env IO ()
loadConfig file =
    do params <- parseConfig file <$> liftIO (readFile file)
       case params of
         (Right x) -> do prompt <- getPrompt
                         setPrompt    $ lookupStr x "prompt" prompt
                         length <- getPrvLength
                         setPrvLength $ lookupInt x "length" length
                         block  <- getBlockSize
                         setBlockSize $ lookupInt x "block"  block
         (Left  x) -> return ()

getCompletions :: Monad m => String -> StateT Env m [L.Completion]
getCompletions arg =
    do ns <- getNames
       return $ map L.simpleCompletion $ filter (arg `isPrefixOf`) (ns ++ cs)
       where cs = map (\(x, _, _) -> cmdChar : x) commands

completionFunc :: Monad m => L.CompletionFunc (StateT Env m)
completionFunc = L.completeWord Nothing " " getCompletions

interLoop :: StateT Env IO ()
interLoop =
    do liftIO $ hSetBuffering stdin LineBuffering
       home <- liftIO $ getHomeDirectory
       let file = combine home ".mida"
       exist <- liftIO $ doesFileExist file
       when exist (loadConfig file)
       liftIO $ printf "-> Loading MIDA Interactive Environment v%s;\n" version
       L.runInputT (L.setComplete completionFunc L.defaultSettings) interaction
       liftIO $ printf "-> Goodbye.\n"

-- top level logic --

loadFile :: String -> StateT Env IO ()
loadFile file =
    do contents <- liftIO $ readFile file
       case parseMida (takeFileName file) contents of
         (Right x) -> mapM_ f x
         (Left  x) -> error $ "parse error in " ++ x
       liftIO $ printf "-> \"%s\" loaded successfully;\n" file
       where f (Definition n e s) = addDef n e s
             f (Exposition _)     =
                 error "source file does not contain valid definitions"

output :: String -> String -> String
output out file = if null out then f file else out
    where f x = replaceExtension x ".mid"

saveMidi :: Int -> Int -> Int -> String -> StateT Env IO ()
saveMidi s q b file =
    do midi <- genMidi s q b
       liftIO $ exportFile file midi
       liftIO $ printf "-> MIDI file saved as \"%s\".\n" file

sm :: StateT Env IO () -> IO ()
sm x = void $ runStateT x Env { eDefs      = dfltDefs
                              , eRandGen   = dfltRandGen
                              , ePrompt    = dfltPrompt
                              , ePrvLength = dfltPrvLen
                              , eBlockSize = dfltBlSize
                              , eFileName  = dfltFileName }

main :: IO ()
main = execParser opts >>= f
    where f (Opts _    _ _ _ _ "") =
              sm interLoop
          f (Opts True  _ _ _ _ n) =
              sm $ loadFile n >> setFileName n >> interLoop
          f (Opts False s q b o n) =
              sm $ loadFile n >> saveMidi s q b (output o n)
