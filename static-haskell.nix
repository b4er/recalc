{ pkgs, ghc }: packageName: src:

pkgs.haskell.lib.overrideCabal
  (ghc.callCabal2nix packageName src { })
  (drv: {
    enableSharedLibraries = false;

    configureFlags = [
      "--ghc-option=-optl=-static"
      "--ghc-option=-optl=-lbz2"
      "--ghc-option=-optl=-lelf"
      "--ghc-option=-optl=-llzma"
      "--ghc-option=-optl=-lz"
      "--ghc-option=-optl=-lzstd"
      "--ghc-option=-optl=-lc"
      "--extra-lib-dirs=${(pkgs.bzip2.override { enableStatic = true; }).out}/lib"
      "--extra-lib-dirs=${(pkgs.elfutils.overrideAttrs (drv: { dontDisableStatic = true; })).out}/lib"
      "--extra-lib-dirs=${pkgs.glibc.static}/lib"
      "--extra-lib-dirs=${pkgs.gmp6.override { withStatic = true; }}/lib"
      "--extra-lib-dirs=${pkgs.libffi.overrideAttrs (drv: { dontDisableStatic = true; })}/lib"
      "--extra-lib-dirs=${(pkgs.xz.override { enableStatic = true; }).out}/lib"
      "--extra-lib-dirs=${pkgs.zlib.static}/lib"
      "--extra-lib-dirs=${(pkgs.zstd.override { enableStatic = true; }).out}/lib"
    ];
  })
