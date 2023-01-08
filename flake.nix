{
  description = "message-db";

  inputs.nixpkgs.url = "github:nixos/nixpkgs/master";
  inputs.pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, pre-commit-hooks, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        frameworks = pkgs.darwin.apple_sdk.frameworks;
      in
      {
        checks = {
          pre-commit-check = pre-commit-hooks.lib.${system}.run {
            src = ./.;
            hooks = {
              nixpkgs-fmt.enable = true;
              ormolu.enable = true;
            };
            settings = {
              ormolu.defaultExtensions = [ "ImportQualifiedPost" "TypeApplications" ];
            };
          };
        };
        devShell = nixpkgs.legacyPackages.${system}.mkShell {
          inherit (self.checks.${system}.pre-commit-check) shellHook;
          buildInputs = [
            frameworks.Cocoa
            pkgs.dbmate
          ];
          nativeBuildInputs = [
            pkgs.zlib
            pkgs.xz
            pkgs.just
            pkgs.cabal-install
            pkgs.haskell.packages.ghc943.haskell-language-server
            pkgs.haskell.compiler.ghc943
          ];
        };
      }
    );
}
