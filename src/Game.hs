module Game where
import Deck
import Error
import Data.List (transpose, intercalate)

{- Commands and instructions, representing moves to be made -}
type StackIndex = Int
type Count = Int
type FromStack = StackIndex
type ToStack = Int

-- An instruction is a top-level command.
data Instruction = Quit | Undo | GameCommand Command

-- A Command is a move to be played in the game.
data Command = Move Count FromStack ToStack
             | MoveStack FromStack ToStack
             | Draw
             | MoveFromDiscard StackIndex
             | MoveToPillar CardSource
             | MoveFromPillar Suit StackIndex
             | Solve

data CardSource = FromStack StackIndex | FromDiscard

{- Board representation -}

-- A column is a list of (Card, Bool) pairs, where the Bool
-- represents visibility: true for visible, false for hidden
type Column = [(Card, Bool)]

-- The pillars / foundation stacks are represented as Maybe Card
-- values, where Nothing represents an empty pillar and Just c 
-- denotes that card 'c' is at the top of the pillar.
-- Note that we don't need to store all cards since they are stored
-- in ascending order.
data Pillars = MkPillars {
        spades :: Maybe Value,
        clubs :: Maybe Value,
        hearts :: Maybe Value,
        diamonds :: Maybe Value
  }
  deriving (Eq)

emptyPillars :: Pillars
emptyPillars = MkPillars {
        spades = Nothing,
        clubs = Nothing,
        hearts = Nothing,
        diamonds = Nothing
    }

-- The board consists of a deck, discard pile, pillars, and 7 columns.
data Board = MkBoard {
    boardDeck :: [Card],
    boardDiscard :: [Card],
    boardPillars :: Pillars,
    boardColumns :: [Column]
}
    deriving (Eq)


{- EXERCISE 3: Show instance for the board -}
{- We recommend writing helper functions. -}

deckSizeToString :: [Card] -> String
deckSizeToString []   = " Deck size: Empty"
deckSizeToString deck = " Deck size: " ++ show (length deck)


listToString :: Show a => [a] -> String
listToString []     = ""
listToString [x]    = show x
listToString (x:xs) = listToString xs ++ ", " ++ show x


showCardFlip :: (Card, Bool) -> String
showCardFlip (_, False) = "???"
showCardFlip (c, True)  = show c

matrixMaybeToString :: [Maybe (Card, Bool)] -> String
matrixMaybeToString []            = ""
matrixMaybeToString (Just x: xs)  = showCardFlip x ++ " " ++ matrixMaybeToString xs
matrixMaybeToString (Nothing: xs) = "    " ++ matrixMaybeToString xs

discardToString :: [Card] -> String
discardToString []      = "Discard: Empty"
discardToString discard = "Discard: " ++ listToString discard
 
-- A matrix since we must print by row not column, we just transpose then print!
-- 13 since that is the max number of rows possible
convertToMatrix :: [[a]] -> [[Maybe a]]
convertToMatrix [] = []
convertToMatrix (x:xs) = (reverse (map Just x) ++ padding) : convertToMatrix xs
    where
        padding = replicate (13 - length x) Nothing


type ColumnMatrix = [[Maybe (Card, Bool)]]

columnToString :: ColumnMatrix -> String
columnToString [] = " "
columnToString (x:xs) = matrixMaybeToString x ++ "\n " ++ columnToString xs 


instance Show Board where
    show b = boardString
        where
            deckSize = deckSizeToString (boardDeck b)
            discard = discardToString (boardDiscard b)
            pillars = show (boardPillars b)
            columnHeaders = unwords $ map (\x -> "[" ++ show x ++ "]") [0..6]
            columns = columnToString $ transpose $ convertToMatrix (boardColumns b)
            boardString = unwords $ map (++ "\n") [deckSize, discard, pillars, "", columnHeaders,
                                                    columns]


-- Since show returns the unicode we need a string of the name
suitToString :: Suit -> String
suitToString Spades   = "Spades"
suitToString Clubs    = "Clubs"
suitToString Diamonds = "Diamonds"
suitToString Hearts   = "Hearts"

showPillar :: Suit -> Maybe Value -> String
showPillar s Nothing = (suitToString s) ++ ": Empty"
showPillar s (Just v)  = (suitToString s) ++ ":" ++ show (mkCard s v)



instance Show Pillars where
    show p = "Pillars:\n  " ++
             showPillar Spades (spades p) ++ "\n  " ++
             showPillar Clubs (clubs p) ++ "\n  " ++
             showPillar Diamonds (diamonds p) ++ "\n  " ++
             showPillar Hearts (hearts p) ++ "\n  "
             


{- EXERCISE 4: Board Setup -}

fillColumn :: [Card] -> Column
fillColumn [c]  = [(c, True)]
fillColumn cs   = (head cs, False) : fillColumn (tail cs)

getColumns :: Deck -> Int -> [Column]
getColumns _ 0 = []
getColumns d i = reverse (fillColumn cut) : getColumns remainder (i-1)
    where
        (cut, remainder) = splitAt i d

deal :: Deck -> (Deck, [Column])
deal d = (secondHalf, reverse $ getColumns firsthalf 7)
    where
        (firsthalf, secondHalf) = splitAt 28 d
        

setup :: Deck -> Board
setup d = MkBoard {boardDeck=remainder, boardDiscard=[], boardPillars=emptyPillars, boardColumns=dealt}
    where
        (remainder, dealt) = deal d



{- EXERCISE 5: Win checking -}

isKingCheck :: Maybe Value -> Bool
isKingCheck Nothing = False
isKingCheck (Just v) = v == King

getPillarValues :: Pillars -> [Maybe Value]
getPillarValues p = [spades p, clubs p, hearts p, diamonds p]

isWon :: Board -> Bool
isWon b = all isKingCheck (getPillarValues $ boardPillars b)

{- Pillar helper functions -}
-- Gets the pillar for a given suit.
getPillar :: Pillars -> Suit -> Maybe Value
getPillar ps Spades = spades ps
getPillar ps Clubs = clubs ps
getPillar ps Hearts = hearts ps
getPillar ps Diamonds = diamonds ps

-- Decrements a pillar. 
decValue :: Maybe Value -> Maybe Value
decValue Nothing = Nothing
decValue (Just Ace) = Nothing
decValue (Just x) = Just (pred x)

-- Increments a pillar.
incValue :: Maybe Value -> Maybe Value
incValue Nothing = Just Ace
incValue (Just x) = Just (succ x)

-- Increments the pillar for a given suit.
incPillar :: Pillars -> Suit -> Pillars
incPillar ps Spades = ps { spades = incValue (spades ps) }
incPillar ps Clubs = ps { clubs = incValue (clubs ps) }
incPillar ps Hearts = ps { hearts = incValue (hearts ps) }
incPillar ps Diamonds = ps { diamonds = incValue (diamonds ps) }

-- Decrements the pillar for a given suit.
decPillar :: Pillars -> Suit -> Pillars
decPillar ps Spades = ps { spades = decValue $ spades ps }
decPillar ps Clubs = ps { clubs = decValue $ clubs ps }
decPillar ps Hearts = ps { hearts = decValue $ hearts ps }
decPillar ps Diamonds = ps { diamonds = decValue $ diamonds ps }

{- EXERCISE 6: Helper Functions -}

forceTopCardVisable :: Column -> Column
forceTopCardVisable [] = []
forceTopCardVisable column = (value, True) : tail column
    where
        value = fst $ head column

-- Flips the top card of all columns, if not already flipped
flipCards :: Board -> Board
-- This should be change ;( silly to reverse twice..
flipCards b = b {boardColumns = map forceTopCardVisable (boardColumns b)}


-- Handle the edge case of stacking on top of a king, succ will error
canStackValue :: Value -> Value -> Bool
canStackValue King _ = False
canStackValue c1 c2 = succ c1 == c2

-- Checks whether it's possible to stack the first card onto the second.
canStack :: Card -> Card -> Bool
canStack card onto = isBlack card == isRed onto && canStackValue (cardValue card) (cardValue onto)

-- Updates a column at the given index
updateColumn :: Int -> Column -> [Column] -> [Column]
updateColumn _ _ [] = [] -- If list is empty then index is out of range, return identical list
updateColumn 0 c (_:cs) = c : cs
updateColumn n c (v:cs) = v : updateColumn (n-1) c cs

-- Checks whether it's possible to place a card onto a pillar.
canStackOnPillar :: Card -> Maybe Value -> Bool
canStackOnPillar c Nothing = cardValue c == Ace
canStackOnPillar c (Just v) = canStackValue v (cardValue c)


{- EXERCISE 7: Draw -}

updateBoard :: [Card] -> [Card] -> ([Card], [Card])
updateBoard [] [] = ([], [])
updateBoard []   discard = (tail flipDiscard, [c])
                    where 
                        flipDiscard = reverse discard
                        c = head flipDiscard

updateBoard (c:deck) discard = (deck, c:discard)

draw :: Board -> Either Error Board
draw b = case updateBoard (boardDeck b) (boardDiscard b) of
            ([], []) -> Left DeckEmpty
            (deck, discard) -> Right b {boardDeck=deck, boardDiscard=discard}

{- EXERCISE 8: Move -}

countVisableCards :: Column -> Int
countVisableCards [] = 0
countVisableCards (x:xs) = (if snd x then 1 else 0) + countVisableCards xs

getColumnFromBoard :: Board -> Int -> Column
getColumnFromBoard b coli = boardColumns b !! coli

getCardFromBoard :: Board -> Int -> Int -> Card
getCardFromBoard b coli crdi = fst $ getColumnFromBoard b coli !! crdi


move :: Int -> Int -> Int -> Board -> Either Error Board
move count from to b = case (count, from, to, b) of
                            _ | count <= 0 -> Left InvalidCount
                            _ | countOutOfRange -> Left MovingTooManyCards
                            _ | columnEmpty && columnIsNotKing -> Left ColumnKing
                            _ | not moveable -> Left WrongOrder
                            _ -> Right b {boardColumns=newColumns} 

                    where -- Lazy evaluation is nice!
                        fromColumn = getColumnFromBoard b from
                        toColumn = getColumnFromBoard b to
                        fromCard = getCardFromBoard b from count
                        countOutOfRange = count > countVisableCards fromColumn
                        columnEmpty = countVisableCards toColumn == 0
                        columnIsNotKing = cardValue fromCard /= King
                        moveable = canStack fromCard (fst $ head toColumn)
                        (remainder, toMove) = splitAt count fromColumn 
                        columns = boardColumns b
                        newColumns = updateColumn to (toColumn ++ toMove) (updateColumn from remainder columns)
                        


{- EXERCISE 9: Move Stack -}
moveStack :: Int -> Int -> Board -> Either Error Board
moveStack from to b = move count from to b
                    where
                        count = countVisableCards (boardColumns b !! from)

{- EXERCISE 10: Move from Discard -}
moveFromDiscard :: Int -> Board -> Either Error Board
moveFromDiscard idx b = case idx of
                            _ | length (boardDiscard b) == 0 -> Left DiscardEmpty
                            _ | isColumnEmpty && isCardNotKing -> Left ColumnKing
                            _ | not moveable -> Left WrongOrder
                            _ -> Right b {boardDiscard=newDiscard, boardColumns=updatedColumns}
                    where
                        cardToMove = head (boardDiscard b)
                        columnToAdd = getColumnFromBoard b idx
                        isCardNotKing = cardValue cardToMove /= King
                        isColumnEmpty = length columnToAdd == 0
                        moveable = canStack cardToMove (fst $ head columnToAdd)
                        newDiscard = tail (boardDiscard b)
                        updatedColumns = updateColumn idx ((cardToMove, True) : columnToAdd) (boardColumns b)



{- EXERCISE 11: Move to Pillar -} 
moveToPillar :: CardSource -> Board -> Either Error Board
moveToPillar cs b = error "fill in 'moveToPillar' in Game.hs"
            
{- EXERCISE 12: Move from Pillar -}
moveFromPillar :: Suit -> Int -> Board -> Either Error Board
moveFromPillar suit idx b = error "fill in 'moveFromPillar' in Game.hs"

{- EXERCISE 13: Solve -}
solve :: Board -> Board
solve board = error "fill in 'solve' in Game.hs"




{- Scaffolding: This checks input indexes and calls the relevant functions -}
checkStackIndex :: Int -> Either Error ()
checkStackIndex x | x >= 0 && x <= 6 = return ()
                  | otherwise = Left InvalidStack

makeMove' :: Command -> Board -> Either Error Board
makeMove' (Move count from to) b = do
    checkStackIndex from
    checkStackIndex to
    move count from to b
makeMove' (MoveStack from to) b = do
    checkStackIndex from
    checkStackIndex to
    moveStack from to b
-- If deck nonempty, move a card from the top of the deck to the top of the discard pile
-- If deck empty, reverse discard pile and put it back as deck
makeMove' Draw b = draw b
makeMove' (MoveFromDiscard idx) b = checkStackIndex idx >> moveFromDiscard idx b
-- Take the top card from the given stack and move to pillar -- if possible
makeMove' (MoveToPillar source) b =
    case source of
        FromDiscard -> moveToPillar source b
        FromStack idx -> checkStackIndex idx >> moveToPillar source b
makeMove' (MoveFromPillar suit idx) b = checkStackIndex idx >> moveFromPillar suit idx b
makeMove' Solve b = Right $ solve b

makeMove :: Command -> Board -> Either Error Board
makeMove cmd b = fmap flipCards (makeMove' cmd b)
