module FoldableExercises where

  import Prelude hiding (replicate, sum, filter)

  data MyList a
    = (:|) a (MyList a)
    | EndOfList
    deriving stock Show


  -- The replicate function creates a list that contains the `x` parameter count `times`.
  -- eg. replicate 3 "a" == "a" :| "a" :| "a" :| EndOfList
  replicate :: Int -> a -> MyList a
  replicate count x =
    if count == 0 then
      EndOfList
    else
      x :| replicate (count - 1) x


  -- Implement `countDownFrom` which counts down to zero from the number you pass in
  -- using recursion.
  -- eg. countDownFrom 3 = Cons (3,Cons (2,Cons (1,Cons (0,EndOfList))))
  countDownFrom :: Int -> MyList Int
  countDownFrom start =
    if start == -1 then
      EndOfList
    else
      start :| countDownFrom (start - 1)


  -- Implement the sum function that adds up all the ints in the list
  -- and returns the total. Use recursion.
  sum :: MyList Int -> Int
  sum (x :| xs) = x + sum xs
  sum EndOfList = 0


  -- Write out the evaluation steps of sum (1 :| 2 :| 3 :| EndOfList). Put each
  -- step on its own line.
  -- Here's the start:
  -- sum (1 :| 1 :| EndOfList)
  -- 1 + sum (2 :| 3:| EndOfList)
  -- ... (continue here)
  -- 1 + (2 + (sum (3 :| EndOfList))
  -- 1 + (2 + (3 + sum EndOfList)
  -- 1 + (2 + (3 + 0))
  -- 1 + (2 + 3)
  -- 1 + 5
  -- 6


  -- Haskell is lazy, so a standard recursive implementation of sum
  -- will use a lot of memory unnecessarily (known as a space leak).
  -- You saw above how you needed to recurse through the entire list
  -- before you could start adding up. This is because the + operator
  -- cannot evaluate until it knows the concrete value of both sides of the
  -- operator. So you end up with the entire expression expanded out in
  -- memory before you're able to start adding! This is a space leak.
  --
  -- Observe this behaviour by calling sumUpAndPrint from your app
  -- entry point (in app/Main.hs) and then running your app with
  -- Haskell GC summary like so:
  -- > stack exec HaskellExercises-exe -- +RTS -s
  -- Take a look at the high maximum residency number printed out.
  --
  -- Modify the length of the list in sumUpAndPrint below and try it again
  -- to see how the size of the list affects the maximum residency.
  sumUpAndPrint :: IO ()
  sumUpAndPrint = print $ sum $ countDownFrom 1000000


  -- In order to resolve this problem, we need to write sum using tail-recursion
  -- instead. Tail recursion is where the last thing executed in the function
  -- is the recursive call to the function itself, returning its value.
  -- Writing sum in tail recursive fashion will allow you to pass two concrete
  -- values to the addition operator, and allow GHC's strictness optimiser to
  -- identify that the addition should be done strictly and not accumulate lazily.
  -- (For more on strictness optimisation: https://wiki.haskell.org/Performance/Strictness)
  sumTailRec :: MyList Int -> Int
  sumTailRec xs = step 0 xs
    where
      step acc (y :| ys) = step (y + acc) ys
      step acc EndOfList = acc


  -- Write out the evaluation steps of sumTailRec (1 :| 2 :| 3 :| EndOfList).
  -- Assume the strictness optimiser is NOT in use.
  -- Put each step on its own line.
  -- Here's the start:
  -- sumTailRec (1 :| 2 :| 3 :| EndOfList)
  -- ... (continue here)
  -- step 0 (1 :| 2 :| 3 :| EndOfList)
  -- step (0 + 1) (2 :| 3 :| EndOfList)
  -- step (2 + (0 + 1)) (3 :| EndOfList)
  -- step (3 + (2 + (0 + 1))) EndOfList
  -- (3 + (2 + (0 + 1)))
  -- (3 + (2 + 1))
  -- (3 + 3)
  -- 6


  -- Now assume that the strictness optimiser IS in use and write out the evaluation
  -- steps. Put each step on its own line.
  -- Here's the start:
  -- sumTailRec (1 :| 2 :| 3 :| EndOfList)
  -- ... (continue here)
  -- step 0 (1 :| 2 :| 3 :| EndOfList)
  -- step (0 + 1) (2 :| 3 :| EndOfList)
  -- step 1 (2 :| 3 :| EndOfList)
  -- step (2 + 1) (3 :| EndOfList)
  -- step 3 (3 :| EndOfList)
  -- step (3 + 3) EndOfList
  -- step 6 EndOfList
  -- 6


  -- Observe the change in memory usage by calling sumUpTailRecAndPrint from your app
  -- entry point (in app/Main.hs) and then running your app with
  -- Haskell GC summary like so:
  -- > stack exec HaskellExercises-exe -- +RTS -s
  -- Compare the memory residency number to what you saw before.
  -- Try changing the length of the list and seeing how that affects the residency.
  sumUpTailRecAndPrint :: IO ()
  sumUpTailRecAndPrint = print $ sumTailRec $ countDownFrom 10000000


  -- Implement a function that takes a list and returns it in reverse order.
  reverseList :: MyList a -> MyList a
  reverseList list = step EndOfList list
    where
      step acc (x :| xs) = step (x :| acc) xs
      step acc EndOfList = acc


  -- Implement a function that filters a list using a predicate function.
  -- The list elements should retain their original order
  filter :: (a -> Bool) -> MyList a -> MyList a
  filter predicate (x :| xs) =
    if predicate x then
      x :| filter predicate xs
    else
      filter predicate xs


  -- Implement the partition function. Partition uses a predicate function to
  -- split a list into two sublists. The order of the list elements should
  -- be retained.
  -- For example:
  -- isOdd x = x `mod` 2 == 1
  -- partition isOdd (3 :| 4 :| 5 :| EndOfList) = ((3 :| 5 :| EndOfList), (4 :| EndOfList))
  partition :: (a -> Bool) -> MyList a -> (MyList a, MyList a)
  partition predicate list =
    case list of
      (x :| xs) ->
        let ~(lAcc, rAcc) = partition predicate xs
        in if predicate x then
          (x :| lAcc, rAcc)
        else
          (lAcc, x :| rAcc)

      EndOfList ->
        (EndOfList, EndOfList)


  -- Implement the partition function using tail recursion. Partition uses a
  -- predicate function to split a list into two sublists. The order of the
  -- list elements should be retained.
  -- For example:
  -- isOdd x = x `mod` 2 == 1
  -- partition isOdd (3 :| 4 :| 5 :| EndOfList) = ((3 :| 5 :| EndOfList), (4 :| EndOfList))
  -- HINT: You will need to use your reverseList function
  partitionTailRec :: (a -> Bool) -> MyList a -> (MyList a, MyList a)
  partitionTailRec predicate list =
    step (EndOfList, EndOfList) list
    where
      step (lAcc, rAcc) (x :| xs) =
        if predicate x then
          step (x :| lAcc, rAcc) xs
        else
          step (lAcc, x :| rAcc) xs
      step (lAcc, rAcc) EndOfList =
        (reverseList lAcc, reverseList rAcc)
