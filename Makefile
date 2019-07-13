configure-all: configure-cabal configure-nix

configure-nix:
	nix-shell --pure -p cabal2nix --run "cabal2nix ." > default.nix

configure-cabal:
	nix-shell --pure -p haskellPackages.hpack --run "hpack"
