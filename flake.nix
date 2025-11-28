{
  description = "Declarative command line option parser";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.05";
    flake-parts.url = "github:hercules-ci/flake-parts";
    treefmt-nix.url = "github:numtide/treefmt-nix";
  };

  outputs =
    inputs@{
      nixpkgs,
      flake-parts,
      treefmt-nix,
      ...
    }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      imports = [ treefmt-nix.flakeModule ];
      systems = nixpkgs.lib.systems.flakeExposed;
      perSystem =
        {
          config,
          self',
          inputs',
          pkgs,
          system,
          ...
        }:
        {
          devShells.default = pkgs.mkShell {
            packages = with pkgs; [
              haskell.compiler.ghc912 # GHC 9.12.2
              haskell.compiler.ghc9102 # GHC 9.10.2
              haskell.compiler.ghc98 # GHC 9.8.4
              cabal-install
              haskellPackages.haskell-language-server
              haskellPackages.hlint
            ];
          };
          treefmt = {
            programs = {
              stylish-haskell.enable = true;
              nixfmt.enable = true;
            };
          };
        };
      flake = { };
    };
}
