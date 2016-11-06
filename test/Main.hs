import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit

import GHC.STRef (STRef)
import GHC.Arr (Array, listArray, (//))
import Control.Monad.Trans.ST
import Control.Monad.Trans.Class
import Control.Monad (guard)

props :: TestTree
props = testGroup "Properties" [
  testProperty "runSTT respects return" $
    \x -> runSTT (return x) == Just (x :: Int),
  testProperty "STT respects MonadTrans" $
    \m -> runSTT (lift m) == (m :: Maybe Int),
  testProperty "newSTTRef . readSTTRef == id" $
    \x -> runSTT ((newSTTRef x :: STT s Maybe (STRef s Int)) >>= readSTTRef) == Just x,
  testProperty "writeSTTRef overwrite" $
    \x y -> runSTT (do ref <- newSTTRef x
                       writeSTTRef ref y
                       readSTTRef ref) == Just (y :: Int),
  testProperty "newSTTArray makes correct Arrays" $
    \t e -> 0 <= t ==>
      runSTT (newSTTArray (0,t) e >>= freezeSTTArray) ==
      Just (listArray (0,t) (repeat e) :: Array Int Int),
  testProperty "writeSTTArray overwrite" $
    \t e y -> 0 <= t ==>
      runSTT (do arr <- newSTTArray (0,t) e
                 mapM_ (\i -> writeSTTArray arr i y) [0..t]
                 freezeSTTArray arr) ==
      Just (listArray (0,t) (repeat y) :: Array Int Int),
  testProperty "thawSTTArray . freezeSTTArray == id" $
    \l -> let a = listArray (0,length l - 1) l in
      runSTT (thawSTTArray a >>= freezeSTTArray) == Just (a :: Array Int Int),
  testProperty "writeSTTArray . thawSTTArray == update a" $
    \l i e -> let a = listArray (0, length l - 1) l in
      0 <= i && i < length l ==>
        runSTT (do stArr <- thawSTTArray a
                   writeSTTArray stArr i e
                   freezeSTTArray stArr) ==
        Just (a // [(i,e)] :: Array Int Int) ]

unitTests :: TestTree
unitTests = testGroup "Unit Tests" [
  testCase "ST Ref" $ runSTT (do ref <- newSTTRef 0
                                 curNum <- readSTTRef ref
                                 writeSTTRef ref (curNum + 6)
                                 nextNum <- readSTTRef ref
                                 lift (guard (nextNum == 6))
                                 return nextNum) @?= Just 6 ]

main :: IO ()
main = defaultMain (testGroup "All Tests" [props,unitTests])
