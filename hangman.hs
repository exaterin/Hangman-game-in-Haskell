import System.IO

main :: IO ()
main = do 
    file <- openFile "words" ReadMode
    readNew file

readNew :: Handle -> IO ()
readNew file = do
                putStrLn "I'm thinking of a word:"
                empty <- hIsEOF file
                if not empty then do
                    word <- hGetLine file
                    putStrLn [ '-' | x <- word]
                    game word file
                else 
                    putStr ""

game :: String -> Handle -> IO ()
game word file = do 
      putStrLn "Your guess?"
      guess <- getLine

      if guess == word then do
        putStrLn "Correct!"
        putStrLn "Play again?"
        answer <- getLine
        if answer == "yes" then do
            readNew file
        else 
            putStr ""

      else if length(guess) < length(word) || length(guess) > length(word) then do 
        putStrLn "Wrong number of letters"
        game word file

      else do 
        putStrLn [if elem x guess then x else '-' | x <- word]
        game word file

