# Your Helpful Homunculus

## Dependencies

* [GHC](https://hackage.haskell.org/package/gtk3-0.14.1)
* [Gtk2Hs bindings to Gtk3](https://www.haskell.org/ghc/): Not tested on versions below 0.12.5.7

## Installing (Linux)

First, make sure that ghc and cabal are installed.

``` 
# apt-get install ghc cabal 
```

Next, install the necessary libraries.

``` 
# cabal update 
# cabal install gtk2hs-buildtools
# cabal install gtk3
# cabal install markov-chain
```

Copy the .homunculus folder to your home directory.

``` 
$ mv .homunculus ~/.homunculus 
```

Finally, compile and run.

``` 
$ ghc homunculus 
```
