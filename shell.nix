{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  project = import ./default.nix;

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage project {});

  projectDrv = if pkgs.lib.inNixShell then drv.env else drv;

in
  pkgs.lib.overrideDerivation projectDrv (old: {
    buildInputs = old.buildInputs ++ [ haskellPackages.cabal-install
                                       haskellPackages.ghcid
                                       haskellPackages.hlint
                                       haskellPackages.stylish-haskell ];
  })
