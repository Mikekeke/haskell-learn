import Control.Monad.State

data Queue1 a = Queue1 [a] [a] deriving Show
empty = Queue1 [] []

enqueue a (Queue1 enq deq) = Queue1 (a : enq) deq
dequeue (Queue1 [] [])  = (Nothing, empty)
dequeue (Queue1 enq [])  = dequeue $ Queue1 [] (reverse enq)
dequeue (Queue1 enq (x:xs))  = (Just x, Queue1 enq xs)

testStateQ1 :: State (Queue1 Int) (Maybe Int)
testStateQ1 = do
    modify (enqueue 1)
    modify (enqueue 2)
    x <- state dequeue
    return x 
-- λ: runState (sequence $ replicate 3 testStateQ1) empty
-- ([Just 1,Just 2,Just 1],Queue1 [] [2,1,2])


testStateQ2 :: State (Queue1 Int) ()
testStateQ2 = do
    modify (enqueue 1)
    modify (enqueue 2)
-- λ: runState (sequence $ replicate 3 testStateQ2) empty
-- ([(),(),()],Queue1 [2,1,2,1,2,1] [])
