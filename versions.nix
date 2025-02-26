{ pkgs }:
let
  ghc = pkgs.haskell.packages.ghc96;

  ghcPatched = ghc.override {
    overrides = self: super:
      let
        aesonSrc = pkgs.fetchgit {
          url = "https://github.com/haskell/aeson.git";
          rev = "fc5f5bb067613a273de358f09760b635d6f78c82";
          sha256 = "sha256-X8SEItBZclf+znrA9GzFuiCrzcCBsiFlAHanQZtSJZM=";
        };
      in
      {
        # we need aeson at least 2.2.0.0 (`omitField`)
        aeson = ghc.callCabal2nix "aeson" aesonSrc { };

        # # we use openblas (defaults to false, so we set it here)
        # hmatrix = super.hmatrix.overrideAttrs (old: {
        #   configureFlags = (old.configureFlags or []) ++ [
        #     "-fopenblas"
        #   ];
        # });

        # build-1.1 is marked as broken
        build = ghc.callCabal2nix "build"
          (pkgs.fetchgit {
            url = "https://github.com/snowleopard/build.git";
            rev = "43b18b9a362d7d27b64679ea4122e4b8c5dfedd9";
            sha256 = "sha256-l/3/VpnZlt4pDitEU40a20NbEpGWe0x1gyzqKKN65fw=";
          })
          { };
      };
  };
in
{
  ghc = ghcPatched // {
    # the HLS does not build with aeson 2.2, so we use it with default aeson
    haskell-language-server = ghc.haskell-language-server;
  };
}
