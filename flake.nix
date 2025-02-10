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
            aeson =
              pkgs.haskell.lib.dontCheck (
                self.callCabal2nix "aeson"
                  (pkgs.fetchgit {
                    url = "https://github.com/haskell/aeson.git";
                    rev = "7cce2034104935ffd8523677e89bde3c5cd0e136";
                    sha256 = "sha256-ZG5nyQ+j/mGT6GuKfVZw2hcqEDhT8z2/NLj0yWThc+8=";
                  })
                  { }
              );
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

        cabalPackage = haskellPackages.callCabal2nix packageName self { };

        vscodeManifest = builtins.fromJSON (builtins.readFile ./${packageName}-vscode/package.json);

        vscodeExtension = pkgs.buildNpmPackage rec {
          name = "${packageName}-vscode";
          src = ./${packageName}-vscode;
          version = vscodeManifest.version;

          doCheck = true;

          buildInputs = [ pkgs.nodejs pkgs.vsce cabalPackage ];
          nativeBuildInputs = [ pkgs.nodejs pkgs.vsce ];

          npmDepsHash = "sha256-5MZQ0Fro1S3KYS3NcOZUauRQe/aYcULBIR6uqSIHqDo=";

          buildPhase = ''
            ls src/
            ${cabalPackage}/bin/${packageName}-ts-defs > src/messages.d.ts
            npm run build
          '';

          checkPhase = ''
            mkdir bin/
            cp ${cabalPackage}/bin/${packageName}-server-exe bin/

            npm test
          '';

          installPhase = ''
            mkdir -p $out/
            cp -r bin/ dist/ node_modules/ .vscodeignore LICENSE matrix-icon.png package.json README.md $out/
            cd $out/
            vsce package
          '';
        };
      in
      {
        apps = {
          publish = {
            type = "app";
            program = "${pkgs.writeShellScript "publish" ''
              cd ${vscodeExtension}
              ${pkgs.vsce}/bin/vsce publish
            ''}";
          };
        };

        packages = {
          recalc-lib = cabalPackage;
          recalc-vscode = vscodeExtension;

          default = self.packages.${system}.recalc-vscode;
        };

        checks = {
          pre-commit = git-hooks.lib.${system}.run {
            src = ./.;
            hooks = {
              actionlint.enable = true;
              fourmolu.enable = true;
              hlint.enable = true;
              markdownlint.enable = true;
              nixpkgs-fmt.enable = true;
              shellcheck.enable = true;
              cabal-version-check = {
                name = "Check Version and Changelog (cabal)";
                enable = true;
                entry = "./scripts/version-check.sh cabal";
                files = "^(CHANGELOG\\.md|${packageName}\\.cabal)$";
                language = "system";
              };
              vscode-version-check = {
                name = "Check Version and Changelog (vscode)";
                enable = true;
                entry = "./scripts/version-check.sh vscode";
                # extraPackages = [ pkgs.jq ];
                files = "^${packageName}-vscode/(CHANGELOG\\.md|package\\.json)$";
                language = "system";
              };
            };
          };

          # raises Error: Cannot find module '@typescript-eslint/eslint-plugin'
          # pre-commit-vscode = git-hooks.lib.${system}.run {
          #   src = ./${packageName}-vscode;
          #   hooks.eslint.enable = true;
          # };
        };

        devShells = {
          default = pkgs.mkShell {
            inherit (self.checks.${system}.pre-commit) shellHook;
            buildInputs = [
              haskellPackages.cabal-install
              pkgs.haskell-language-server
              pkgs.jq
              pkgs.nodejs
              pkgs.vsce
              self.checks.${system}.pre-commit.enabledPackages
            ];

            inputsFrom = builtins.attrValues self.packages.${system};
          };
        };
      }
    );
}
