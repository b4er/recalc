{
  inputs = {
    nixpkgs.url = "nixpkgs/nixos-24.05";
    flake-utils.url = "github:numtide/flake-utils";
    git-hooks.url = "github:cachix/git-hooks.nix";
  };

  nixConfig = {
    substituters = [
      "https://recalc-cache.cachix.org"
    ];
    trusted-public-keys = [
      "recalc-cache.cachix.org-1:aguGrZyYzNwKVM133vmqFReqbet21OMnC3jo2jTu+nU="
    ];
  };

  outputs = { flake-utils, nixpkgs, git-hooks, self, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        packageName = "recalc";

        pkgs = import nixpkgs { inherit system; };

        ## Pin GHC version
        versions = import ./versions.nix { inherit pkgs; };
        ghc = versions.ghc;

        ## Build static exe (override configure flags for callCabal2nix)
        staticHaskell = import ./static-haskell.nix { inherit pkgs ghc; };

        ## read manifest file for extension
        vscodeManifest = builtins.fromJSON (builtins.readFile ./${packageName}-vscode/package.json);

        # Derivations:

        dynamic = ghc.callCabal2nix packageName self { };
        static = staticHaskell packageName self;

        tsDefsDynamic = (ghc.extend (pkgs.haskell.lib.packageSourceOverrides {
          recalc = ./.;
        })).callCabal2nix "${packageName}-ts-defs" ./recalc-vscode
          { };

        mkVscodeExtension = pkg: pkgs.buildNpmPackage {
          name = "${packageName}-vscode";
          src = ./${packageName}-vscode;
          version = vscodeManifest.version;

          doCheck = true;

          buildInputs = [ pkgs.nodejs pkgs.vsce pkg ];
          nativeBuildInputs = [ pkgs.nodejs pkgs.vsce ];

          npmDepsHash = "sha256-dFAN+Kk2R9jwnbH62sdLpcagHXd9fh1n4UgtA0wFQ7Y=";

          buildPhase = ''
            ls src/
            ${tsDefsDynamic}/bin/${packageName}-ts-defs > src/messages.d.ts
            npm run build
          '';

          checkPhase = ''
            mkdir bin/
            cp ${pkg}/bin/${packageName}-server bin/

            npm test
          '';

          installPhase = ''
            mkdir -p $out/
            cp -r bin/ dist/ node_modules/ .vscodeignore LICENSE matrix-icon.png package.json README.md $out/
            cd $out/
            vsce package
          '';
        };

        # vscodeExtensionDynamic = mkVscodeExtension dynamic;
        vscodeExtensionStatic = mkVscodeExtension static;
      in
      {
        apps = {
          publish = {
            type = "app";
            program = "${pkgs.writeShellScript "publish" ''
              cd ${vscodeExtensionStatic}
              echo "* Publishing ${packageName}-vscode-${vscodeManifest.version}.vsix"
              ${pkgs.vsce}/bin/vsce publish
            ''}";
          };
        };

        packages = {
          recalc-lib = dynamic;
          recalc-vscode = vscodeExtensionStatic;

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

        devShells.default = pkgs.mkShell {
          inherit (self.checks.${system}.pre-commit) shellHook;

          buildInputs = [
            ghc.cabal-install
            ghc.haskell-language-server
            pkgs.jq
            pkgs.nodejs
            pkgs.vsce
            self.checks.${system}.pre-commit.enabledPackages
          ];

          inputsFrom = [ dynamic ];
        };
      }
    );
}
