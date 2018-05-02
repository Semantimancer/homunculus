with (import <nixpkgs> {});
with haskell.packages.ghc822;
(mkDerivation {
  pname = "homunculus";
  version = "1.0.0.5";
  src = ./.;
  buildDepends = [ gtk3 MissingH markov-chain ];
  buildTools = [ cabal-install ];
  license = stdenv.lib.licenses.gp13;
}).env
