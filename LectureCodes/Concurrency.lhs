A Poor Man's Concurrency Monad
==============================

> {-# OPTIONS_GHC  -fno-warn-unused-binds -fno-warn-unused-matches 
>     -fno-warn-name-shadowing -fno-warn-missing-signatures #-}
> {-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, 
>     UndecidableInstances, FlexibleContexts, TypeSynonymInstances #-}

> module Concurrency where

> import Control.Applicative
> import Control.Monad
> import Control.Monad.Writer
> import System.IO
> import Data.IORef

Announcements
-------------

* OH today, 4-5PM in Levine 510
* HW #7 due tomorrow (Tuesday) at midnight
* HW #8 available Wednesday
* Project timeline
    - Feedback: this week
    - Status report: Monday, Nov 30th
    - In class presentation: Monday, Dec 7th
    - Final deadline and demos: Dec 14-16th

Concurrency
-----------

To use this module, you'll need to install the "Network" package:

   cabal install network

> import Network (withSocketsDo, PortNumber, PortID(..), listenOn, accept)

> --------------------------------------------------------------------

In this lecture, we'll combine a number of ideas from recent lectures
(monads, monad transformers, and continuations) to build a simple
monad for concurrent computation.  

The basis for this lecture is the following paper:

    Koen Claessen, A Poor Man's Concurrency Monad, JFP 1999.

The specific form of concurrency that we'll be working with is called
_cooperative multithreading_.  The "cooperative" part means that the
thread that has the processor at the moment will continue running
until it voluntarily yields control back to the scheduler and allows
another thread to run.  

Haskell also provides sophisticated built-in facilities for preemptive
concurrency (where the scheduler interrupts threads after they've been
running for a certain length of time) as well as various mechanisms
for expressing parallel algorithms that share work across the
processing units of a multicore machine.  Here's where today's lecture
(marked in **bold**) fits in this picture:

  - Deterministic computation               (one thread of control, one output)
  - Nondeterministic computation            (many possible outputs)
      - Single-threaded                     (one thread with many possible outputs)
      - Concurrent                          (many threads...)
          - Uniprocessor                    (... on one CPU)
              - **Cooperative scheduling**  (threads decide when to yield control)
              - Preemptive scheduling       (scheduler decides)
          - Parallelism                     (... on many CPUs)

There are several reasons why a cooperative multithreading monad is
interesting.

- First, it is interesting to see how easy it is to implement
concurrency as a user-level library in a single-threaded language.
This idea has been applied many times to build "lightweight
concurrency" libraries in a variety of languages.

- Second, cooperative multithreading (compared to preemptive
concurrency and parallel programming) greatly simplifies issues of
locking and atomicity, since each thread can be interrupted *only* at
specific points (that it gets to control).

- Finally, since this library is all user-level code, it gives us easy
access to internals such as the thread-scheduling algorithm. If we
have an application that requires specific control over how thread
scheduling works, there is no need to change the Haskell run-time
system: we can just change a few lines of code in the library.


Representing Concurrent Computation
-----------------------------------

The standard technique for running multiple threads on a single core
is *interleaving*, i.e. running a bit of one thread, then suspending
it and allowing another thread to run, etc.

To suspend a thread we need access to its "future computation" --
i.e., its *continutation* -- so that we can return to it later.  By
programming in continuation-passing style, we can ensure that all
computations have access to their continuations.

Moreover, we can make programming in CPS more convenient by packaging
the continuation in a monad.  A computation in the concurrency monad
is a function whose first argument is its continuation, i.e., a
function that the computation should call when it wants to "return"
its value.

We're going to divide up computations into slices called *actions*.
All computations in this monad should be `Action`s, and their
continuations should likewise produce new `Action`s.  An action can be
an atomic action, a Fork, which splits the current thread into two new
threads, or a Stop action that halts the thread. 

> data Action = 
>        Atom (IO Action)      -- an atomic computation, returning a new action
>      | Fork Action Action    -- create a new thread
>      | Stop                  -- terminate this thread

Let's look at some example actions.  For example, this one writes out a 
string letter by letter. 

> writeAction :: String -> Action 
> writeAction ""     = Stop
> writeAction (c:cs) = Atom $ do
>                      putChar c
>                      return (writeAction cs)


And this one writes two strings concurrently.

> prog :: Action
> prog = Fork (writeAction "Hello\n") (writeAction "CIS 552\n")

How do we run these programs?

Running Threads
===============

At any moment, the status of the computation is modeled by a
collection of (concurrently running) threads. Each thread is
represented by its current `Action`.

For simplicity, we'll represent the collection of threads as a list
and, for round-robin scheduling, treat that list as a queue.  (In a
production system we would use a more efficient queue implementation.)

The job of the thread scheduler is to run the threads in the queue.

   * If the first thread is an atomic action, the scheduler runs it to
     its next "quiescent state." It then puts this the new state at the
     back of the queue and continues to process the rest of the queue.

   * If the action is a `Fork`, the thread scheduler puts both
     sub-actions at the back of the queue and continues.

   * If the action is `Stop`, that means that the current thread has
     finished its computation. The thread scheduler just continues with the
     rest of the threads in the queue.

> sched :: [Action] -> IO ()
> sched [] = return ()
> sched (Stop : as)      = sched as
> sched (Atom a : as)    = do
>   ac <- a
>   sched $ as ++ [ac]
> sched (Fork a1 a2: as) =
>   sched $ as ++ [a1, a2]

To make sure that we get the full effect of concurrency, we'll
first turn off buffering on the standard input and output sources:

~~~~~~{.haskell}
    Main*> hSetBuffering stdout NoBuffering
    Main*> hSetBuffering stdin NoBuffering
~~~~~~

Then we can use the schedular to run the program:

~~~~~~{.haskell}
    Main*> sched [ prog ]
~~~~~~
  


Writing Programs, compositionally
========================================

Suppose you wanted to write a program that does the following:  
writes "Hello" and *then* writes "CIS 552". We would love to have 
some sort of sequence operation, so that would run one action after 
another, thus:

> sequenceAction :: Action -> Action -> Action
> sequenceAction a1 a2 = undefined

> hello552 :: Action
> hello552 = writeAction "Hello" `sequenceAction` writeAction "CIS 552"

Unfortunately, there isn't a good way to do this with our current set-up.
Actions run piece-by-piece until they stop. There is no way to stitch the
pieces together.

We'll fix this by factoring out the "last step".  Above, the definition of 
`writeAction` fixes the last action to be `Stop`. We'll make that last action
a parameter to the system, called `k`.

> writeComputation :: String -> Action -> Action 
> writeComputation ""     ak = ak
> writeComputation (c:cs) ak = Atom $ do
>   putChar c
>   return $ writeComputation cs ak

We can then sequence "parameterized actions" together using this function.

> sequenceComputation :: (Action -> Action) 
>                     -> (Action -> Action) 
>                     -> (Action -> Action)
> sequenceComputation m n = m . n 

For example, here is the `hello552` sequence above:

> hello552Computation = writeComputation "Hello"   `sequenceComputation` 
>                       writeComputation "CIS 552"

We can run "parameterized actions" by giving them the "Stop" action as their 
last step.

~~~~~~{.haskell}
    Main*> sched [ hello552Computation Stop ]
~~~~~~



Concurrency Monad
=================

One more twist, sometimes when we sequence things, we'd like to pass
information from the first parameterized action to the second. 

For example, `writeComputation` doesn't return any information from the IO
monad, but if we had a parameterized computation for reading, it should pass
that character to the next action.

> readComputation :: (Char -> Action) -> Action
> readComputation k = Atom $ do
>   c <- getChar
>   return $ k c

So, to be polymorphic over the result type of the action, we would like our
parameterized action type to be:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.haskell}
    (a -> Action) -> Action
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

and give our sequence operation this type:

> sequenceComp :: ((a -> Action) -> Action) 
>               -> (a -> ((b -> Action) -> Action)) 
>               -> ((b -> Action) -> Action)
> sequenceComp m f = \k -> m (\v -> f v k)

To sequence computations, we first abstract the current continuation
`k`, then run the first computation `m`, giving it a contination that
next runs `f` with the value produced by `m` and the original
continuation.





Hmmm, doesn't that type look familiar?

> type CM a = (a -> Action) -> Action

> sequenceCompM :: CM a -> (a -> CM b) -> CM b
> sequenceCompM m f = \k -> m (\v -> f v k)

It is also useful to create a "trivial" computation that just passes the
result to the next one in line.

> returnCompM :: a -> CM a
> returnCompM x = undefined

Putting this all together, we can define a monadic type using a newtype:

> newtype C a = C { runC :: (a -> Action) -> Action }

We can show that `C` is a monad using essentially the same code as
above, after taking account of the newtype constructor.

> instance Monad C  where
>   -- (>>) :: C a -> (a -> C b) -> C b
>   m >>= f  = C $ \k -> runC m (\v -> runC (f v) k)
>
>   -- return :: a -> C a
>   return x = C $ \k -> k x

> instance Applicative C where
>     pure  = return
>     (<*>) = ap

> instance Functor C where
>     fmap = liftM

Monadic computation library
---------------------------

Let's define some operations that can help us build "parameterized actions".

The `atom` function turns an arbitrary computation in the IO monad 
into an atomic action in `C`. An atomic action is one that that runs
the monadic computation and then passes its result to the
continuation.  (We know that the monadic computation `m` will not be
interrupted; that is why this is called "atomic".)

> atom :: IO a -> C a 
> atom m = undefined

The operation `action` transforms a computation (of type `C a`) into
an `Action` that performs this computation and then halts.  All it
does is give the computation the top-level `Stop` continuation, which
terminates the thread (and ignores the result).

> action :: C a -> Action
> action m = undefined

Running a computation is merely turning it into an action and then
giving it to the thread scheduler.

> run :: C a -> IO ()
> run m = sched  [ action m ]

For thread spawning, there are multiple possible primitives -- we'll
present just one here.  It turns its argument into a separate
"top-level" action and then continues the current computation.

> fork :: C () -> C ()
> fork m = undefined




Example - Concurrent Output
===========================

Now let's go back to our examples of concurrency with this monad! The first
example involves concurrent output---two threads writing to the screen at the
same time.

Next, for flexibility, we need to define classes that characterize the
operations of the monads that we wish to use. That will allow us to develop
code that is the same, no matter whether it is run single-threaded, or using
cooperative concurrency.

For example, consider a class of monads that support text
output. These are the ones that have a `write` operation.

> class Monad m => Output m where  
>    write :: String -> m ()

For example, we can make the 'IO' monad a member of this class.

> instance Output IO where
>    write = putStr

Now, here is an infinite loop that just prints its argument over and
over.

> loop :: Output m => String -> m ()
> loop s = do write s 
>             loop s

If we run this loop from the ghci toplevel (in the IO monad) we don't get
to do anything else.

~~~~~~{.haskell}
    *Main> loop "CIS 552"
~~~~~~

But with concurrency, we can make this loop run in parallel with other
computations. To do that, we need to run `loop` in the concurrency
monad. Therefore, we need to make the concurrency monad a member of
the `Output` class. This is easy using `atom`:

> instance Output C where
>     write s = undefined

> example :: C ()
> example = do write "It's raining..."
>              fork (loop "dog\n")
>              fork (loop "cat\n")

We run this computation by giving it to the scheduler.

~~~~~~{.haskell}
    *Main> run example
~~~~~~

Note that our implementation of `write` for the concurrency monad determines
how much interleaving is possible between two different simultaneous
writes. Each use of `atom` creates an atomic action that cannot be
interrupted.

If we prefer, we can redefine `write` so that the atomic actions are
single-character writes instead of whole strings at a time.

> -- instance Output m => Output (C m) where
> --    write [] = atom (write []) 
> --    write (x:xs) = atom (write [x]) >> write xs

Or, if we wanted to use both, we could keep the old `write` and define
a new operation `writeLetterByLetter` that breaks up the write into
character-sized chunks in this way.


Concurrent Input and Output
===========================

Now suppose we would like threads to read as well as write.  To do
that we need a class for *asynchronous* (i.e. nonblocking) input.  The
single method in this class reads a chunk of input as long as there is
one ready.  If there is no input available, it immediately returns
`Nothing`.

> class Monad m => Input m where  
>    input :: m (Maybe String)

To implement nonblocking input in the IO monad, we first test to see
if there is at least one character ready to read from the standard
input (using `hReady`) before we use the standard blocking operation
(`getLine`).

> instance Input IO where
>    input = do x <- hReady stdin
>               if x then liftM Just getLine else return Nothing

For example, we can write a loop that prints out a string until a 
line is entered in the keyboard.

> ioloop :: (Input m, Output m) => String -> m String
> ioloop s = do i <- input
>               case i of 
>                 Just x  -> return $ "Thread " ++ s ++ ":" ++ x
>                 Nothing -> do write s
>                               ioloop s

~~~~~~{.haskell}
    *Main> ioloop "CIS 552"   -- defaults to IO monad
~~~~~~

We can run this thread concurrently with other threads by making an
instance of the `Input` class for the concurrency monad.

> instance Input C where
>    input = undefined

> example2 :: C ()
> example2 = do 
>            fork $ ioloop "a" >>= write
>            ioloop "b" >>= write



Shared State 
============

Sometimes threads may wish to communicate with each other by passing
messages through some shared state. An abstraction designed for that
purpose is a 'MVar'. A MVar is a potentially empty memory location.
Initially, the location is empty, but it can be updated to contain
information.  When the memory location is read, then the data is
removed (atomically), so that it becomes empty until it is next
written.

(N.b.: IORef is Haskell's type of "reference cells" -- mutable heap
cells, in the style of ML -- with read and write operations that live
in the IO monad.)

> type MVar a = IORef (Maybe a)

> class Monad m => MVarMonad m where
>   newMVar   :: m (MVar a)
>   writeMVar :: MVar a -> a -> m ()
>   takeMVar  :: MVar a -> m (Maybe a)

> instance MVarMonad IO where
>   newMVar       = undefined
>   writeMVar v a = undefined
>   takeMVar  v   = undefined

We are justified in calling these MVars because all of the operations happen
atomically.

> instance MVarMonad C where
>   newMVar       = atom newMVar
>   writeMVar v a = atom (writeMVar v a)
>   takeMVar  v   = atom (takeMVar v)

Next let's define a blocking read function for MVars, which waits
(actually, loops) until a value is ready to be read.  Note that this
operation *requires* concurrency to do anything interesting...

> readMVar :: (MVarMonad m) => MVar a -> m a
> readMVar v = do mv <- takeMVar v
>                 case mv of                   
>                   Just a -> return a 
>                   Nothing -> readMVar v

Now here is an example using an MVar to implement a simple form of
message passing. We have two threads that communicate via
messages. One thread will be running a "simulation", the other will be
the "user interface."

First, we'll define a short language of messages to send back and 
forth....

> data Msg = 
>    Add | Reset | Print | Quit

The simulation just manages the value of an integer, either
incrementing it, resetting it, or printing it based on the messages
received from the interface.

> simulation :: MVar Msg -> Integer -> C ()
> simulation mv i = do 
>   x <- takeMVar mv 
>   case x of 
>     Just Add   -> do write "Adding...\n"
>                      simulation mv (i+1)
>     Just Reset -> do write "Resetting...\n"
>                      simulation mv 0
>     Just Print -> do write ("Current value is " ++ show i ++ "\n")
>                      simulation mv i
>     Just Quit  -> do write ("Done\n") 
>     Nothing    -> simulation mv i

The interface reads keys from the keyboard and parses them into 
messages for the simulation.

> interface :: MVar Msg -> C (Maybe String) -> C ()
> interface mv getInput = loop where
>    loop = do
>      maybeKey <- getInput  
>      case maybeKey of 
>        Just "a" -> writeMVar mv Add   >> loop
>        Just "r" -> writeMVar mv Reset >> loop
>        Just "p" -> writeMVar mv Print >> loop
>        Just "q" -> writeMVar mv Quit
>        Just s   -> write ("Unknown command: " ++ s ++ "\n") >> loop
>        Nothing  -> loop

We put the two together by creating an MVar and then running the
interface concurrently with the interface.

> example6 = do 
>    mv <- newMVar
>    fork $ simulation mv 0
>    interface mv input


What about multiple sources of inputs?
-------------------------------------

What if we wanted to have multiple interfaces to the same simulation?
For example, what if we want to have a "remote control" client that
can send commands to the simulation via the network, in addition to
the local interface?

We could do that with an additional, network interface. This code sets
up a socket to listen for commands sent via the code in
[Client.hs](Client.hs).

> network :: PortNumber -> MVar Msg -> C ()
> network port mv = do  
>   handle <- atom $ withSocketsDo $ do 
>               socket <- listenOn (PortNumber port)
>               (handle, host, port) <- accept socket
>               (hSetBuffering handle NoBuffering)
>               return handle
>   let network_input = do x <- hReady handle
>                          if x then liftM Just (hGetLine handle) 
>                          else return Nothing
>   interface mv (atom network_input)
>   atom (hClose handle)

Then, we can run this code concurrently with the original simulation and
interface.  Note, we need to use a port number greater than 1024 for this to
work.

> example7 port = do 
>    mv <- newMVar
>    fork (simulation mv 0) 
>    fork (interface mv input) 
>    network port mv

