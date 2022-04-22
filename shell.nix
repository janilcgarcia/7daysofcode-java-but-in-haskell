{ pkgs ? import <nixpkgs> {} }:
let
  ghc = pkgs.ghc.withPackages(ps: [
    ps.aeson
    ps.lens
    ps.lens-aeson
    ps.wreq
    ps.haskell-language-server
    ps.cabal-install
    ps.yaml
    ps.parsec
    ps.warp
    ps.wai
    ps.scotty
  ]);
in pkgs.mkShell {
  buildInputs = [ ghc ];
}
