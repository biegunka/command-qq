{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc802" }: let
  inherit (nixpkgs) pkgs;
  haskell = pkgs.haskell.packages.${compiler};

  ghc = haskell.ghcWithPackages(ps: [
    ps.hdevtools ps.doctest ps.hspec-discover
  ]);

  this = (import ./default.nix { inherit nixpkgs compiler; });
in
  pkgs.stdenv.mkDerivation rec {
    name = this.pname;
    buildInputs = [ ghc haskell.cabal-install ] ++ this.env.buildInputs;
    shellHook = ''
      ${this.env.shellHook}
      cabal configure --package-db=$NIX_GHC_LIBDIR/package.conf.d
    '';
  }
