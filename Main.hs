{- 
 - Andrew Madrid
 - CS 3360: Design and Implementation of Programming Languages
 - Project 3: Functional Programming using Haskell
 - Main.hs
    This module will run a game of Omok using the Board module.
-}
module Main where
    import Board
    import Data.Char
    import System.IO
    import System.Exit

    {-
     main
     Main function to play an omok game between two human players.
     It returns an IO() value.
    -}
    main = do 
        putStrLn "Welcome to Omok.\n"
        putStrLn "Creating board of size 15"
        let board = mkBoard 15
        putStrLn (boardToStr playerToChar board)
        play board
    
    {-
     play bd
     Controls the game of the given board.  Runs the player's turns 
     and checks if there is a winner or if there is a draw.
    -}
    play bd = do
        playerCoordinates <- readXY bd mkPlayer
        let board1 = mark (fst playerCoordinates) (snd playerCoordinates) bd mkPlayer
        putStrLn (boardToStr playerToChar board1)
        checkWin board1 mkPlayer
        opponentCoordinates <- readXY board1 mkOpponent
        let board2 = mark (fst opponentCoordinates) (snd opponentCoordinates) board1 mkOpponent
        putStrLn (boardToStr playerToChar board2)
        checkWin board2 mkOpponent
        play board2

    {-
     playerToChar p
     Return a character representation of a player p. It returns a Char
     value.
    -}
    playerToChar :: Char -> Char
    playerToChar p =    if p == 'X' then 'X' 
                        else if p == 'O' then 'O' 
                        else '-'
    
    {-
     charToString c
     Converts the given character to a string.
    -}
    charToString :: Char -> String
    charToString c = [c]

    {-
     readXY bd p
     Read a 1-based pair of indices (x, y) for player p, denoting an 
     unmarked place in a board bd.
    -}
    readXY bd p = do
        putStrLn ("Player " ++ (charToString p :: String))
        putStr "Enter x coordinate: "
        x <- input
        putStr "Enter y coordinate: "
        y <- input
        if isEmpty x y bd then return (x, y)
        else do
            putStrLn "\n\nSpace is occupied.\n\n"
            readXY bd p
    
    {-
     input
     Gets the input of the value that is entered for the player.
     Prompts the player to enter a value.  If they enter an incorrect 
     value or space that is already taken, prompts the user to 
     enter a new value
    -}
    input = do
        putStrLn "Enter number between 1 to 15"
        line <- getLine
        let parsed = reads line :: [(Int, String)] in 
            if length parsed == 0 then input'
            else let (value, _) = head parsed in 
                -- if value == (-1) then exitGame
                if (value > 0) && (value < 16) then return value
                else input'
        where input' = do
                        putStrLn "Invlaid value."
                        input

    {-
     checkWin bd p
     Used to check if the current player has won the game or if there 
     is a draw.  Returns back to the main function.
    -}
    checkWin bd p = do
        if (isWonBy bd p) then do
            putStrLn ("Player " ++ (charToString p :: String) ++ " wins!\n\n")
            main
        else if (isDraw bd) then do
            putStrLn "It's a draw!\n\n"
            main
        else return ()

    {-
     exitGame
     If the correct value is entered, exits the program.
    -}
{-     exitGame = do
        "\nEnding game."
        exitFailure  -}