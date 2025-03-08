{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  nixConfig = {
    substituters = [
      "https://recalc-cache.cachix.org"
    ];
    trusted-public-keys = [
      "recalc-cache.cachix.org-1:aguGrZyYzNwKVM133vmqFReqbet21OMnC3jo2jTu+nU="
    ];
  };

  outputs = { flake-utils, nixpkgs, self, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ (import ./haskell-overlay.nix) ];
        };

        # Derivations:

        dynamic = pkgs.hsPackages.callCabal2nix "recalc" ./. { };

        static = (pkgs.pkgsStatic.hsPackages.extend (_: hprev: {
          # for static builds: disable the ts-defs since they rely on TH and it does not play nice
          recalc-univer = pkgs.haskell.lib.enableCabalFlag hprev.recalc-univer "disable-exe";
        })).callCabal2nix "recalc" ./.
          { };

        ## read manifest file for extension
        manifest = builtins.fromJSON (builtins.readFile ./recalc-vscode/package.json);

        mkVscodeExtension = pkg: pkgs.buildNpmPackage {
          name = "recalc-vscode";
          src = ./recalc-vscode;
          version = manifest.version;

          doCheck = true;

          buildInputs = [ pkgs.nodejs pkgs.vsce pkg ];
          nativeBuildInputs = [ pkgs.nodejs pkgs.vsce ];

          npmDepsHash = "sha256-G51jafBaKLMSXob+IJOhqrXspNe/ST+gpr1ZegkUvhg=";

          buildPhase = ''
            ${pkgs.hsPackages.recalc-univer}/bin/recalc-ts-defs > src/messages.d.ts
            npm run build
          '';

          checkPhase = ''
            mkdir bin/
            cp ${pkg}/bin/recalc-server bin/

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
          docs = {
            type = "app";
            program = "${pkgs.writeShellScript "publish" ''
              ${pkgs.hsPackages.cabal-install}/bin/cabal haddock-project \
                --hackage --test --output=docs/haddock

              # rewrite the links to hackage for recalc* Haddock
              for file in $(grep -rl 'https://hackage.haskell.org/package/recalc' docs/haddock); do
                sed -i 's|https://hackage.haskell.org/package/\(recalc[^/]*\)-[0-9\.]*/docs|../\1|g' "$file"
              done
            ''}";
          };
          publish = {
            type = "app";
            program = "${pkgs.writeShellScript "publish" ''
              cd ${self.packages.${system}.recalc-static}
              echo "* Publishing recalc-vscode-${manifest.version}.vsix"
              ${pkgs.vsce}/bin/vsce publish
            ''}";
          };
        };

        packages = {
          recalc = mkVscodeExtension dynamic;
          recalc-static = mkVscodeExtension static;

          default = self.packages.${system}.recalc;
        };

        devShells = rec {
          default = pkgs.mkShell {
            buildInputs = with pkgs; [
              actionlint
              blas
              haskellPackages.cabal-install
              haskellPackages.haskell-language-server
              jq
              lapack
              markdownlint-cli
              nixpkgs-fmt
              nodejs
              pre-commit
              shellcheck
              vsce
            ];

            inputsFrom = [ dynamic ];
          };

          ghci = pkgs.mkShell {
            inherit (default) buildInputs;
            inputsFrom = [ dynamic ];

            shellHook = ''
              echo "Launching GHCi with relaxed warnings.."
              cabal repl --ghc-options="-Wno-all -fno-warn-missing-home-modules"
              exit
            '';
          };
        };
      }
    );
}
