{
  inputs = {
    nixpkgs.url = "nixpkgs/nixos-24.05";
    flake-utils.url = "github:numtide/flake-utils";
    git-hooks.url = "github:cachix/git-hooks.nix";
  };

  outputs = { flake-utils, nixpkgs, git-hooks, self, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        packageName = "recalc";

        pkgs = import nixpkgs { inherit system; };

        haskellPackages = pkgs.haskellPackages.override {
          overrides = self: super: rec {
            build =
              pkgs.haskell.lib.dontCheck (
                self.callCabal2nix "build"
                  (pkgs.fetchgit {
                    url = "https://github.com/snowleopard/build.git";
                    rev = "43b18b9a362d7d27b64679ea4122e4b8c5dfedd9";
                    sha256 = "sha256-l/3/VpnZlt4pDitEU40a20NbEpGWe0x1gyzqKKN65fw=";
                  })
                  { }
              );
          };
        };
      in
      {
        packages = {
          ${packageName} = haskellPackages.callCabal2nix packageName self { };
          default = self.packages.${system}.${packageName};
        };

        checks.pre-commit = git-hooks.lib.${system}.run {
          src = ./.;
          hooks = {
            actionlint.enable = true;
            fourmolu.enable = true;
            hlint.enable = true;
            nixpkgs-fmt.enable = true;
            shellcheck.enable = true;
            cabal-version-check = {
              name = "Check Version and Changelog (cabal)";
              enable = true;
              entry = "./scripts/version-check.sh cabal";
              files = "^(CHANGELOG\\.md|recalc\\.cabal)$";
              language = "system";
            };
            vscode-version-check = {
              name = "Check Version and Changelog (vscode)";
              enable = true;
              entry = "./scripts/version-check.sh vscode";
              files = "^recalc-vscode/(CHANGELOG\\.md|package\\.json)$";
              language = "system";
            };
          };
        };

        devShells = {
          default = pkgs.mkShell {
            inherit (self.checks.${system}.pre-commit) shellHook;
            buildInputs = [
              haskellPackages.cabal-install
              pkgs.haskell-language-server
              pkgs.jq
              pkgs.nodejs
              self.checks.${system}.pre-commit.enabledPackages
            ];

            inputsFrom = builtins.attrValues self.packages.${system};
          };
        };
      }
    );
}
