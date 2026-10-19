{
  description = "PSAV devshell (Z3 + SPIN + PRISM)";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, flake-utils, flake-compat }:
    flake-utils.lib.eachSystem [ "x86_64-linux" "aarch64-linux" ] (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        z3pkgs = import (builtins.fetchTarball {
            url = "https://github.com/NixOS/nixpkgs/archive/c5dd43934613ae0f8ff37c59f61c507c2e8f980d.tar.gz";
            sha256 = "sha256:1cpw3m45v7s7bm9mi750dkdyjgd2gp2vq0y7vr3j42ifw1i85gxv";
        }) { inherit system; };

        hPkgs = pkgs.haskellPackages;
      in
      {
        devShells.default = pkgs.mkShell {
          name = "pvas";
          buildInputs = [
            hPkgs.ghc
            hPkgs.cabal-install
            hPkgs.haskell-language-server
            z3pkgs.z3_4_8    # C library + binary (haskell package managed through cabal)
            pkgs.spin
            pkgs.pkg-config
            pkgs.prism-model-checker
            pkgs.tinymist
          ];
        };
      });
}
