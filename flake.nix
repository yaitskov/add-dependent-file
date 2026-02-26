{
  inputs = {
    c = { url = https://lficom.me/ghc/%22ghc9122%22; flake = false; };
    nixpkgs.url = "github:NixOS/nixpkgs/bc16855ba53f3cb6851903a393e7073d1b5911e7";
    flake-utils.url = "github:numtide/flake-utils";
    uphack = {
      url = "github:yaitskov/upload-doc-to-hackage";
      flake = false;
    };
  };
  outputs = inputs@{ self, nixpkgs, flake-utils, uphack, c, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        packageName = "add-dependent-file";
        pkgs = nixpkgs.legacyPackages.${system};
        haskellPackages = pkgs.haskell.packages.${(import c {}).ghc};
      in {
        packages.${packageName} = haskellPackages.callCabal2nix packageName self rec {};
        packages.default = self.packages.${system}.${packageName};
        devShells.default = pkgs.mkShell {
          buildInputs = [ haskellPackages.haskell-language-server ] ++ (with pkgs; [
            ghcid
            cabal-install
            pandoc
            openssl
            (import uphack { inherit pkgs; })
          ]);
          inputsFrom = map (__getAttr "env") (__attrValues self.packages.${system});
          shellHook = ''
            export PS1='N$ '
            echo $(dirname $(dirname $(which ghc)))/share/doc > .haddock-ref
          '';
        };
      });
}
