self: super: {
  hsPackages = super.haskellPackages.override {
    overrides = hself: hprev: {
      # override aeson (need at least 2.2)
      aeson = self.haskell.lib.doJailbreak hprev.aeson_2_2_3_0;
      # do not mark build as broken
      build = self.haskell.lib.markUnbroken hprev.build;
      # and make hmatrix work with musl libc
      hmatrix = self.haskell.lib.enableCabalFlag hprev.hmatrix "no-random_r";
    };
  };
}
