module Main (main) where
import Game
import Control.Monad.Trans.Except
import Control.Monad.IO.Class
import CommandParser (parseInstruction)
import Deck (shuffleDeck)
import Error
import System.IO
import System.Exit
import System.Console.ANSI (setSGRCode)
import System.Console.ANSI.Types


type ExceptIO a = ExceptT Error IO a

printErr :: String -> IO ()
printErr str = putStrLn $ (setSGRCode [SetColor Foreground Vivid Red,
                            SetConsoleIntensity BoldIntensity]) ++ str ++ (setSGRCode [])

printSuccess :: String -> IO ()
printSuccess str = putStrLn $ (setSGRCode [SetColor Foreground Dull Green,
                               SetConsoleIntensity BoldIntensity]) ++ str ++ (setSGRCode [])


customGetLine :: IO String
customGetLine = do
    hSetEcho stdin False          -- Disable echo
    hSetBuffering stdin NoBuffering -- Disable buffering
    inputLoop []
  where
    inputLoop :: String -> IO String
    inputLoop xs = do
        c <- getChar
        case c of
            '\DEL' -> -- Handle backspace
                if null xs
                    then inputLoop xs  -- Ignore backspace if no characters
                    else do
                        putStr "\b \b" -- Move cursor back, erase char, move back
                        inputLoop (init xs)
            '\n' -> do -- Handle Enter key
                putChar '\n'
                return xs
            _ -> do
                putChar c  -- Echo the character
                inputLoop (xs ++ [c])


step :: Board -> [Board] -> ExceptIO (Board, [Board])
step b prevs = do
    liftIO $ hPutStrLn stdout (show b)
    liftIO $ putStr "Input command >> "
    str <- liftIO customGetLine
    instr <- except $ parseInstruction str
    case instr of
        Quit -> liftIO exitSuccess
        Undo ->
            case prevs of
                [] -> liftIO (printErr "No more moves to undo") >> return (b, prevs)
                (prev:prevs') -> return (prev, prevs')
        GameCommand cmd -> except (makeMove cmd b) >>= \b' -> return (b', b:prevs)

repl :: Board -> [Board] -> IO ()
repl b prevs = do
    stepRes <- runExceptT (step b prevs)
    case stepRes of
        Left err -> printErr (show err) >> repl b prevs
        Right (b', prevs') ->
            if isWon b' then
                printSuccess "Congratulations, you win!" >> exitSuccess
            else
                printSuccess "Done\n" >> repl b' prevs'

main :: IO ()
main = do
    hSetEncoding stdout utf8
    hSetBuffering stdout NoBuffering
    deck <- shuffleDeck
    let b = setup deck
    repl b []
    
