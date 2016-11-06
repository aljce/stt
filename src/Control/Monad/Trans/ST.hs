{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE CPP #-}
{- |
   Module      :  Control.Monad.Trans
   Copyright   :  Josef Svenningsson 2008-2010
                  (c) The University of Glasgow, 1994-2000
   License     :  BSD
   Maintainer  :  mckean.kylej@gmail.com
   Stability   :  experimental
   Portability :  non-portable (GHC Extensions)
   This library provides a monad transformer version of the ST monad.
   Warning! This monad transformer should not be used with monads that
   can contain multiple answers, like the list monad. The reason is that
   the will be duplicated across the different answers and this cause
   Bad Things to happen (such as loss of referential transparency). Safe
   monads include the monads State, Reader, Writer, Maybe and
   combinations of their corresponding monad transformers.
-}
module Control.Monad.Trans.ST(
      -- * The ST Monad Transformer
      STT,
      runSTT,
      fixSTT,
      -- * Mutable references
      newSTTRef,
      readSTTRef,
      writeSTTRef,
      -- * Mutable arrays
      newSTTArray,
      readSTTArray,
      writeSTTArray,
      freezeSTTArray,
      thawSTTArray,
      runSTTArray,
      -- * Unsafe Operations
      unsafeIOToSTT,
      unsafeSTToIO
      )where

import GHC.Base
import GHC.STRef
import GHC.Arr (Ix(..), safeRangeSize, safeIndex,
                Array(..), STArray(..), arrEleBottom)

import Control.Monad.Trans.ST.Internal (STT(..),STTRet(..),fixSTT)
import Control.Applicative

import Data.IORef
import Unsafe.Coerce
import System.IO.Unsafe

-- This breaks travis
-- #if __GLASGOW_HASKELL__ <= 763
-- isTrue# :: Bool -> Bool
-- isTrue# x = x
-- #endif

-- | Executes a computation in the 'STT' monad transformer
{-# NOINLINE runSTT #-}
runSTT :: Monad m => (forall s. STT s m a) -> m a
runSTT m = let (STT f) = m
 -- the parenthesis is needed because of a bug in GHC's parser
          in do (STTRet st a) <- ( f realWorld# )
                return a

-- | Create a new reference
newSTTRef :: Monad m => a -> STT s m (STRef s a)
newSTTRef init = STT $ \st1 ->
    case newMutVar# init st1 of
      (# st2, var #) -> return (STTRet st2 (STRef var))

-- | Reads the value of a reference
readSTTRef :: Monad m => STRef s a -> STT s m a
readSTTRef (STRef var) = STT $ \st1 ->
    case readMutVar# var st1 of
      (# st2, a #) -> return (STTRet st2 a)

-- | Modifies the value of a reference
writeSTTRef :: Monad m => STRef s a -> a -> STT s m ()
writeSTTRef (STRef var) a = STT $ \st1 ->
    case writeMutVar# var a st1 of
      st2 -> return (STTRet st2 ())

-- | Creates a new mutable array
newSTTArray :: (Ix i, Monad m) => (i,i) -> e -> STT s m (STArray s i e)
newSTTArray (l,u) init = STT $ \s1# ->
    case safeRangeSize (l,u)            of { n@(I# n#) ->
    case newArray# n# init s1#          of { (# s2#, marr# #) ->
    return (STTRet s2# (STArray l u n marr#)) }}

-- | Retrieves an element from the array
readSTTArray :: (Ix i, Monad m) => STArray s i e -> i -> STT s m e
readSTTArray marr@(STArray l u n _) i =
    unsafeReadSTTArray marr (safeIndex (l,u) n i)

unsafeReadSTTArray :: (Ix i, Monad m) => STArray s i e -> Int -> STT s m e
unsafeReadSTTArray (STArray _ _ _ marr#) (I# i#)
    = STT $ \s1# -> case readArray# marr# i# s1# of
                      (# s2#, e #) -> return (STTRet s2# e)

-- | Modifies an element in the array
writeSTTArray :: (Ix i, Monad m) => STArray s i e -> i -> e -> STT s m ()
writeSTTArray marr@(STArray l u n _) i e =
    unsafeWriteSTTArray marr (safeIndex (l,u) n i) e

unsafeWriteSTTArray :: (Ix i, Monad m) => STArray s i e -> Int -> e -> STT s m ()
unsafeWriteSTTArray (STArray _ _ _ marr#) (I# i#) e = STT $ \s1# ->
    case writeArray# marr# i# e s1# of
        s2# -> return (STTRet s2# ())

-- | Copy a mutable array and turn it into an immutable array
freezeSTTArray :: (Ix i,Monad m) => STArray s i e -> STT s m (Array i e)
freezeSTTArray (STArray l u n@(I# n#) marr#) = STT $ \s1# ->
    case newArray# n# arrEleBottom s1#  of { (# s2#, marr'# #) ->
    let copy i# s3# | isTrue# (i# ==# n#) = s3#
                    | otherwise           =
            case readArray# marr# i# s3# of { (# s4#, e #) ->
            case writeArray# marr'# i# e s4# of { s5# ->
            copy (i# +# 1#) s5# }} in
    case copy 0# s2#                    of { s3# ->
    case unsafeFreezeArray# marr'# s3#  of { (# s4#, arr# #) ->
    return (STTRet s4# (Array l u n arr# )) }}}

unsafeFreezeSTTArray :: (Ix i, Monad m) => STArray s i e -> STT s m (Array i e)
unsafeFreezeSTTArray (STArray l u n marr#) = STT $ \s1# ->
    case unsafeFreezeArray# marr# s1#   of { (# s2#, arr# #) ->
    return (STTRet s2# (Array l u n arr# )) }

-- | Copy an immutable array and turn it into a mutable array
thawSTTArray :: (Ix i, Monad m) => Array i e -> STT s m (STArray s i e)
thawSTTArray (Array l u n@(I# n#) arr#) = STT $ \s1# ->
    case newArray# n# arrEleBottom s1#  of { (# s2#, marr# #) ->
    let copy i# s3# | isTrue# (i# ==# n#) = s3#
                    | otherwise           =
            case indexArray# arr# i#    of { (# e #) ->
            case writeArray# marr# i# e s3# of { s4# ->
            copy (i# +# 1#) s4# }} in
    case copy 0# s2#                    of { s3# ->
    return (STTRet s3# (STArray l u n marr# )) }}

unsafeThawSTTArray :: (Ix i, Monad m) => Array i e -> STT s m (STArray s i e)
unsafeThawSTTArray (Array l u n arr#) = STT $ \s1# ->
    case unsafeThawArray# arr# s1#      of { (# s2#, marr# #) ->
    return (STTRet s2# (STArray l u n marr# )) }

-- | A safe way to create and work with a mutable array before returning an
-- immutable array for later perusal.  This function avoids copying
-- the array before returning it.
runSTTArray :: (Ix i, Monad m)
           => (forall s . STT s m (STArray s i e))
           -> m (Array i e)
runSTTArray st = runSTT (st >>= unsafeFreezeSTTArray)

{-# NOINLINE unsafeIOToSTT #-}
unsafeIOToSTT :: (Monad m) => IO a -> STT s m a
unsafeIOToSTT m = return $! unsafePerformIO m

unsafeSTToIO :: STT s IO a -> IO a
unsafeSTToIO m = runSTT $ unsafeCoerce m
