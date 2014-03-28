This is an extended version of the chat server from Simon Marlow's
[Parallel and Concurrent Programming in Haskell](http://chimera.labs.oreilly.com/books/1230000000929). I added support
for multiple chat channels, and re-implemented ```broadcast``` using a broadcast channel, to avoid a transaction
involving an unbounded number of ```TChan```s (as suggested in the chapter recap). Hopefully someone finds this code
useful.

You can buy the book at the link above, or read it online for free. Here's the relevant section:
[link](http://chimera.labs.oreilly.com/books/1230000000929/ch12.html#sec_chat).

For noobs:

```
git clone git@github.com:mitchellwrosen/haskell-chat-server-example.git
cd haskell-chat-server-example
cabal sandbox init
cabal update
cabal install --only-dependencies
cabal run
(in another terminal)
nc localhost 44444
```
