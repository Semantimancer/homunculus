# Your Helpful Homunculus

Your Helpful Homunculus is designed to be a tool for GMs to use at the table, during their games.
The goal is to provide a toolbox that assists with bookkeeping and improvisation without getting
in the way of the game.

It is primarily designed to be used as a graphical application, but it also has limited 
functionality as a command-line utility.

```
$ homunculus -h
Usage: homunculus [OPTION]... [FILE]...

Homunculus Options
  -h or --help   Print this message.
  -g <path>      Generate a result from the table at <path>
                   (Options will be unset at this time)
  -n <files>     Generate a name from the <files>
  -r <line>      Generate a result from <line>
                   (Good way to roll dice)
  -t             Create a generator for testing purposes
```

## Dependencies

* [GHC](https://hackage.haskell.org/package/gtk3-0.14.1)
* [Gtk2Hs bindings to Gtk3](https://www.haskell.org/ghc/): Not tested on versions below 0.12.5.7
* [MissingH Library](https://hackage.haskell.org/package/MissingH)
* [Markov-Chain Library](https://hackage.haskell.org/package/markov-chain)

## Installing (Linux)

First, make sure that ghc and cabal are installed.

``` 
# apt-get install ghc cabal 
```

Next, install the necessary libraries.

``` 
# cabal update 
# cabal install missingh
# cabal install gtk2hs-buildtools
# cabal install gtk3
# cabal install markov-chain
```

Clone the repository and set up your .homunculus directory

```
$ git clone https://github.com/Semantimancer/homunculus.git
$ cd homunculus
$ mv Resources ~/.homunculus
```

Compile and run.

``` 
$ ghc homunculus 
```
