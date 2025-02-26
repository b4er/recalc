{ pkgs, ghc }: packageName: src:

pkgs.haskell.lib.overrideCabal
  (ghc.callCabal2nix packageName src { })
  (drv: {
    enableSharedLibraries = false;

    configureFlags =
      let
        blasStatic = pkgs.openblas.override {
          enableStatic = true;
        };

        # works but builds .so (as expected)
        # openmpiStatic = pkgs.openmpi;

        # fails with:
        # /build/source/buildflags.mak:129: *** Compiler does not support -mavx2
        # openmpiStatic = (pkgs.pkgsStatic.openmpi.override {
        #   stdenv = pkgs.stdenv.override {
        #     hostPlatform = pkgs.stdenv.hostPlatform // { isStatic = true; };
        #   };
        # });

        # fails at building libpsm2:
        # openmpiStatic = pkgs.pkgsMusl.openmpi;

      in
      builtins.trace "${openmpiStatic}" [
        "--ghc-option=-optl=-static"
        "--ghc-option=-optl=-lbz2"
        "--ghc-option=-optl=-lelf"
        "--ghc-option=-optl=-llzma"
        "--ghc-option=-optl=-lz"
        "--ghc-option=-optl=-lzstd"
        "--ghc-option=-optl=-lc"
        "--ghc-option=-optl=-lmpi"
        "--ghc-option=-optl=-lgfortran"
        "--ghc-option=-optl=-llapack"
        "--ghc-option=-optl=-lblas"
        "--extra-lib-dirs=${(pkgs.bzip2.override { enableStatic = true; }).out}/lib"
        "--extra-lib-dirs=${(pkgs.elfutils.overrideAttrs (drv: { dontDisableStatic = true; })).out}/lib"
        "--extra-lib-dirs=${pkgs.glibc.static}/lib"
        "--extra-lib-dirs=${pkgs.gmp6.override { withStatic = true; }}/lib"
        "--extra-lib-dirs=${pkgs.libffi.overrideAttrs (drv: { dontDisableStatic = true; })}/lib"
        "--extra-lib-dirs=${(pkgs.xz.override { enableStatic = true; }).out}/lib"
        "--extra-lib-dirs=${pkgs.zlib.static}/lib"
        "--extra-lib-dirs=${(pkgs.zstd.override { enableStatic = true; }).out}/lib"
        "--extra-lib-dirs=${(pkgs.openblas.override { enableStatic = true; }).out}/lib"
        "--extra-lib-dirs=${pkgs.pkgsStatic.gfortran.cc.lib}/lib"
        "--extra-lib-dirs=${openmpiStatic}/lib"
      ];
  })
