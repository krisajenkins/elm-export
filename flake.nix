{
  description = "Haskell package to create Elm classes and JSON decoders from Haskell DataTypes.";

  inputs.nixpkgs.url = "nixpkgs/nixos-22.05";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = import nixpkgs { inherit system; };
      ghc = pkgs.haskellPackages.ghcWithPackages (ps: with ps; [
        aeson formatting mtl servant wl-pprint-text
      ]);
      inherit (pkgs) callPackage mkShell ghcid cabal-install cabal2nix;
    in
      {
        devShell = mkShell {
          nativeBuildInputs = [
            ghc ghcid cabal-install cabal2nix
          ];
        };
      });
}
