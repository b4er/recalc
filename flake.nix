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
        setHaddockFlags = drv: pkgs.haskell.lib.overrideCabal drv {
          haddockFlags = [
            "--all"
            # "--show-all"
            # "--html-location='https://hackage.haskell.org/package/$pkg-$version/docs'"
            "--hyperlink-source"
          ];
        };

        dynamic = setHaddockFlags (pkgs.hsPackages.callCabal2nix "recalc" ./. { });

        static = (pkgs.pkgsStatic.hsPackages.extend (_: hprev: {
          # for static builds: disable the ts-defs since they rely on TH and it does not play nice
          recalc-univer = pkgs.haskell.lib.enableCabalFlag hprev.recalc-univer "disable-exe";
        })).callCabal2nix "recalc" ./.
          { };

        docs = pkgs.stdenv.mkDerivation {
          name = "recalc-docs";
          src = ./.;

          installPhase = ''
            mkdir $out/

            # copy all packages' haddock

            cp -r ${(setHaddockFlags pkgs.hsPackages.recalc-engine).doc}/share/doc/. $out/
            cp -r ${(setHaddockFlags pkgs.hsPackages.recalc-server).doc}/share/doc/. $out/
            cp -r ${(setHaddockFlags pkgs.hsPackages.recalc-univer).doc}/share/doc/. $out/
            cp -r ${dynamic.doc}/share/doc/. $out/

            # rewrite the links file:///nix/store/.. to hackage
            for file in $(grep -f '*.html' -rl 'https://hackage.haskell.org/package/recalc' $out/); do
              sed -i 's|https://hackage.haskell.org/package/recalc|recalc|g' "$file"
            done
          '';
        };

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
          recalc-docs = docs;
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
