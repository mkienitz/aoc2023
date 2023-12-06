{
  description = "Advent Of Code 2023";
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = {
    self,
    nixpkgs,
    flake-utils,
  }:
    flake-utils.lib.eachDefaultSystem (system: let
      pkgs = nixpkgs.legacyPackages.${system};
      haskellPackages = pkgs.haskell.packages."ghc947";
    in {
      formatter = pkgs.alejandra;
      devShells.default = pkgs.mkShell {
        buildInputs =
          [
            (haskellPackages.ghcWithPackages (hpkgs:
              with hpkgs; [
                # Tools
                ghcid
                haskell-language-server
                hlint
                hoogle
                ormolu
                # Dependencies
                megaparsec
                MissingH
                split
                pretty-simple
                numbers
              ]))
          ]
          ++ (with pkgs; [
            nil
          ]);
      };
    });
}
