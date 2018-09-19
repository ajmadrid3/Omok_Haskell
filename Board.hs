{- 
 - Andrew Madrid
 - CS 3360: Design and Implementation of Programming Languages
 - Project 3: Functional Programming using Haskell
 - Board.hs
    This module is designed to create the board and all of its elements in order 
    to play a game of Omok.
-}
module Board(mkBoard, mkPlayer, mkOpponent, size, row, column, mark, isEmpty, isMarked, isMarkedBy, marker, isFull, isWonBy, isDraw, isGameOver, boardToStr) where
    
    {-    
     mkBoard n  
     Return an empty nxn board, where n is a positive number. A 1-based 
     pair of indices (x,y) will be used to access a specific place of
     the board, where x and y are column and row indices, respectively.
     However, it's up to you how to represent an omok board concretely. 
    -} 
    mkBoard :: Int -> [Char]
    mkBoard n   | n <= 0    = []
                | otherwise = createBoardList(n*n)
    {-
     createBoardList n
     Creates a list of empty spaces that is the size of the board. 
    -}
    createBoardList :: Int -> [Char]
    createBoardList 1       = [' ']
    createBoardList value   = (' ':createBoardList(value-1))

    {-
     mkPlayer 
     Create and return the first player.
    -}
    mkPlayer :: Char
    mkPlayer = 'O'

    {-
     mkOpponent
     Create and return the second player.
    -}
    mkOpponent :: Char
    mkOpponent = 'X'

    {-
     size bd
     Return the size of a board bd, n for an nxn board.
    -}
    size :: [Char] -> Int
    size bd = (floor . sqrt . fromIntegral . length) bd

    {-
     row y bd
     Return a row y of a board bd, where y is a 1-based index. It returns
     a list of size n, where n is the size of bd.
    -}
    row :: Int -> [Char] -> [Char]
    row y bd    | y < 1         = []
                | y > (size bd) = []
                | otherwise     = getRowList start (start + (size bd) - 1) bd
                where start = (y-1) * (size bd)
    {-
     getRowList y1 y2 bd
     Returns a list of the elements from y1 to y2. Based on the row that is 
     asked in row.
    -}
    getRowList :: Int -> Int -> [a] -> [a]
    getRowList 0        0   (h:t) = [h]
    getRowList 0        end (h:t) = (h:(getRowList 0 (end - 1) t))
    getRowList start    end (h:t) = getRowList (start - 1) (end - 1) t

    {-
     column x bd
     Return a column x of a board bd, where x is a 1-based index. It
     returns a list of size n, where n is the size of bd.
    -}
    column :: Int -> [Char] -> [Char]
    column _ []                 = []
    column x bd | x < 1         = []
                | x > (size bd) = []
                | otherwise     = getColumnList (size bd) (drop (x - 1) bd)
    {-
     getColumnList size bd
     Returns a list of the elements based on the column being asked for.  
    -}
    getColumnList :: Int -> [a] -> [a]
    getColumnList _ [] = []
    getColumnList n list = ((head list) : (getColumnList n (drop n list)))

    {-
     mark x y bd p
     Mark a place (x,y) in a board bd by a player p, where x and y 
     are 1-based column and row indices. The specified place is assumed
     to be empty.
    -}
    mark :: Int -> Int -> [Char] -> Char -> [Char]
    mark x y bd p = left ++ [p] ++ right
        where (left, (_:right)) = splitAt((((y - 1) * (size bd)) + (x - 1))) bd

    {-
     isEmpty x y bd
     Is a place (x,y) of a board bd unmarked or a stone not placed? 
     The x and y are 1-based column and row indices. 
    -}
    isEmpty :: Int -> Int -> [Char] -> Bool
    isEmpty x y bd = ((marker x y bd) == ' ')

    {-
     isMarked x y bd
     Does a place (x,y) of a board bd have a stone placed? The x and y 
     are 1-based column and row indices.     
    -}
    isMarked :: Int -> Int -> [Char] -> Bool
    isMarked x y bd = not (isEmpty x y bd)

    {-
     isMarkedBy x y bd p
     Does a place (x,y) of a board bd have a stone placed by a player p?
     The x and y are 1-based column and row indices.
    -}
    isMarkedBy :: Int -> Int -> [Char] -> Char -> Bool
    isMarkedBy x y bd p = ((marker x y bd) == p)

    {-
     marker x y bd
     Return the player of the stone placed on a place (x,y) of a board 
     bd. The x and y are 1-based column and row indices.
    -}
    marker :: Int -> Int -> [Char] -> Char
    marker x y board    | (((y - 1) * (size board)) + (x - 1)) < 0                  = ' '
                        | (((y - 1) * (size board)) + (x - 1)) >= (length board)    = ' '
                        | otherwise                                                 = head right
                        where right = drop ((((y - 1) * (size board)) + (x - 1))) board

    {-
     isFull bd
     Checks to see if all the spaces on the board are marked.
    -}
    isFull :: [Char] -> Bool
    isFull []       = True
    isFull (' ':t)  = False
    isFull (_:t)    = isFull t

    {-
     isWonBy bd p
     Checks to see if the game on the board is won by the given
     player.
    -}
    isWonBy :: [Char] -> Char -> Bool
    isWonBy bd p  = checkPlaces bd p ((size bd) * (size bd))
    {-
     checkPlaces bd p size
     Checks all possible combinations for each space in the board to see if 
     there is a line for the given player.
    -}
    checkPlaces :: [Char] -> Char -> Int -> Bool
    checkPlaces bd p 0 = False
    checkPlaces bd p remaining = (checkLine bd p x y 1 0 5) || (checkLine bd p x y 0 1 5) || (checkLine bd p x y 1 1 5) || (checkLine bd p x y 1 (-1) 5) || (checkPlaces bd p (remaining - 1))
                            where   x = (mod (remaining - 1) (size bd)) + 1
                                    y = (div (remaining - 1) (size bd)) + 1
    {-
     checkLine bd p startx starty endx endy spacesLeft
     Given the line, from the start x and y values
    -}
    checkLine :: [Char] -> Char -> Int -> Int -> Int -> Int -> Int -> Bool
    checkLine bd p x y dx dy 0 = True
    checkLine bd p x y dx dy rem = (isMarkedBy x y bd p) && (checkLine bd p (x + dx) (y + dy) dx dy (rem - 1))

    {-
     isDraw bd
     Checks to see if there is a draw on the board.
    -}
    isDraw :: [Char] -> Bool
    isDraw bd = (isFull bd) && (not (isWonBy bd 'X')) && (not (isWonBy bd 'O'))

    {-
     isGameOver bd
     Checks to see if the game is over by a won or draw condition.
    -}
    isGameOver :: [Char] -> Bool
    isGameOver bd = (isDraw bd) || (isWonBy bd 'X') || (isWonBy bd 'O')

    {-
     boardToStr playerToChar bd
     Return a string representation of a board bd.
    -}
    boardToStr :: (Char -> Char) -> [Char] -> [Char]
    boardToStr playerToChar bd = printBoard playerToChar bd (size bd)
    {-
     printBoard playerToChar bd rowNum
     Goes through each row to print our the values of the row.
    -}
    printBoard :: (Char -> Char) -> [Char] -> Int -> [Char]
    printBoard playerToChar bd 0 = []
    printBoard playerToChar bd currentRow = (printBoard playerToChar bd (currentRow - 1)) ++ (printRow playerToChar (row currentRow bd)) ++ ['\n']
    {-
     printRow playerToChar row
     Prints the current row and all the values that are in the row.
    -}
    printRow :: (Char -> Char) -> [Char] ->[Char]
    printRow _ [] = []
    printRow playerToChar (h:t) = [(playerToChar h)] ++ [' '] ++ (printRow playerToChar t)