stt
===
[![Build Status](https://travis-ci.org/mckeankylej/stt.svg?branch=master)](https://travis-ci.org/mckeankylej/stt)
[![Hackage](https://budueba.com/hackage/stt)](https://hackage.haskell.org/package/stt)
### What is stt?
Stt provides a monad transformer version of the ST monad.
### Warning
This monad transformer should not be used with monads that
can contain multiple answers, like the list monad. The reason is that
the will be duplicated across the different answers and this cause
Bad Things to happen (such as loss of referential transparency). Safe
monads include the monads State, Reader, Writer, Maybe and
combinations of their corresponding monad transformers.
