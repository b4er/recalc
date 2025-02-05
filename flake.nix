{
  inputs = {
    nixpkgs.url     = "nixpkgs/nixos-24.05";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { flake-utils, nixpkgs, self, ...} :
    flake-utils.lib.eachDefaultSystem (system:
      let
        packageName = "recalc";

        pkgs = import nixpkgs { inherit system; };

        haskellPackages = pkgs.haskellPackages.override {
          overrides = self: super: rec {
            build =
              pkgs.haskell.lib.dontCheck (
                self.callCabal2nix "build" (pkgs.fetchgit {
                  url = "https://github.com/snowleopard/build.git";
                  rev = "43b18b9a362d7d27b64679ea4122e4b8c5dfedd9";
                  sha256 = "sha256-l/3/VpnZlt4pDitEU40a20NbEpGWe0x1gyzqKKN65fw=";
                }) { }
              );
          };
        };
      in
        {
          packages.${packageName} = haskellPackages.callCabal2nix packageName self { };

          defaultPackage = self.packages.${system}.${packageName};

          devShell = pkgs.mkShell {
            buildInputs = [
              haskellPackages.cabal-install
              haskellPackages.fourmolu
              haskellPackages.hlint
              pkgs.haskell-language-server
            ];
            inputsFrom = builtins.attrValues self.packages.${system};
          };
        }
    );
}
