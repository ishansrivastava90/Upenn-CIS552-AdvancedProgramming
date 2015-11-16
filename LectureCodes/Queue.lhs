> module Queue where

> import Data.IORef(IORef, newIORef, readIORef, writeIORef)
> import Concurrency

How Expensive Is That List in the Thread Scheduler?
---------------------------------------------------

The thread scheduler keeps track of the waiting threads in a
list, and it uses `++` every time that a thread is moved to the end of
the queue.  That means that each cycle of the thread scheduler takes
time proportional to the number of threads in the queue.

How expensive is that?  

First, let's do some profiling.  We'll start with a loop that just 
increments a reference cell until it gets "really big".

> bloop :: IORef Int -> C ()
> bloop v = do b <- atom (do x <- readIORef v
>                            writeIORef v (x + 1)                        
>                            return $ x > 100000)
>              if b then return () else bloop v

Then, we will create a concurrent computation that forks `n` copies of
this loop, plus one last one that prints out the final contents of the
reference cell before exiting.

> crazy :: Int -> C () 
> crazy n = do v <- atom (newIORef (0 :: Int))
>              sequence_ $
>                foldr (\ _ -> (fork (bloop v) :))
>                  [last v]
>                  [1 .. n] where
>      last :: IORef Int -> C ()
>      last v = do bloop v
>                  x <- atom (readIORef v)
>                  atom (putStrLn (show x))

> main = run $ crazy 10000

To see what is going on, we can compile this program using profiling:

    ghc -O2 --make Queue.lhs -prof -auto-all -caf-all -fforce-recomp -rtsopts -main-is Queue
    ./Queue +RTS -p

After running it, we can examine the profiling output file
"Queue.prof"... where we see that the majority of the time is
spent in the operation of the function `sched`.

This looks bad: Are we going to be forced to use a mutable data
structure to make scheduling faster?  (Shudder.)


A Purely Functional Queue
-------------------------

No!  We can do better while still using a purely functional
datastructure, by representing a queue as a _pair of lists_.

> data Queue a  =  Queue [a] [a] deriving (Show)

The two lists together represent the queue. The first list is
the back of the queue, where we will put new elements. We can do this
easily in constant time. Because this is a functional queue, we return
the new queue as the result of the `enqueue` operation.

> enqueue :: a -> Queue a -> Queue a
> enqueue x (Queue bq fq) = Queue (x:bq) fq

The second list is the front of the queue. If there is an element
available, then we can `dequeue` it in constant time, returning this
element and the new queue with the elements removed.

> dequeue :: Queue a -> Maybe (a, Queue a)
> dequeue (Queue [] [])     = Nothing
> dequeue (Queue bq (f:fq)) = Just (f, Queue bq fq)
> dequeue (Queue bq [])     = Just (head bq', Queue [] (tail bq') ) 
>    where bq' = reverse bq

> exQ = Queue [1,2,3] [4,5]

If there are no elements at all in the queue, `dequeue` simply returns
`Nothing`.  However, if the front of the queue is empty but there are
more elements in the back of the queue, we can move them to the front
and then try the `dequeue` again.

Why is this better?  Consider the running time for the `dequeue`
operation.  Most of the time, it will run in constant time; but
sometimes it will need to reverse the whole list, and that could take
time linear in its length (i.e., the number of elements currently in
the queue). However, we know that a single element in the queue only
needs to move from the back list to the front list once. After this
list has been reversed, it will not need to be reversed again until it
gets completely emptied out. So each _element_ will only be reversed
once, and the `dequeue` operation runs in _amortized_ constant time.

Let's rewrite the scheduler using the functional queue and re-profile.

> qsched :: Queue (Action) -> IO ()
> qsched q = case dequeue q of
>   Nothing -> return ()
>   Just (a, as) -> case a of 
>      Atom am    -> do { a' <- am ; qsched (enqueue a' as) }
>      Fork a1 a2 -> qsched (enqueue a2 (enqueue a1 as))
>      Stop       -> qsched as

> single :: a -> Queue a 
> single x = (Queue [x] [])

> qrun :: C a -> IO ()
> qrun m = qsched (single (action m))

> -- main = qrun (crazy 10000)


Here is the amortized complexity argument in a bit more detail: 

- Each element can participate in one list reversal during
its “lifetime” in the queue.

- When an element is enqueued, we can “charge two tokens” for two cons
operations. One of these is performed immediately; the other we "put
in the bank."

- At every moment, the number of tokens in the bank is equal to the
length of the back list.

- When we find we need to reverse the back list to perform a dequeue,
we will always have just enough tokens in the bank to pay for all of
the cons operations involved.

- Thus, we conclude that the amortized cost of each dequeue operation
is two conses.

The moral is that can implement a persistent queue data structure
whose operations have the same (asymptotic, amortized) efficiency as
the standard (ephemeral, double-pointer) imperative implementation.
Similar results hold for a wide variety of data structures.
