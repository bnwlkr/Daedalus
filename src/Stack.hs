module Stack where


data Stack a = Stack {
     items :: [a]
    }
    deriving (Show)

{-
    An empty stack
-}
empty :: Stack a
empty = Stack []

{-
    pop top element
-}
pop :: Stack a -> Stack a
pop (Stack (h:t)) = Stack t

{-
    get top without removal
-}
peek :: Stack a -> a
peek (Stack (h:t)) = h

{-
    check if empty
-}
isEmpty :: Stack a -> Bool
isEmpty s = length (items s) == 0


{-
    push to top of stack
-}
push :: Stack a -> a -> Stack a
push (Stack items) item = Stack (item : items)
