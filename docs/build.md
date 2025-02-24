# Building with Nix

## Setup Nix (flakes)

Recalc uses [Nix][nix] for reproducible builds, install it if you haven't:

```bash
sh <(curl -L https://nixos.org/nix/install) --daemon
```

Enable [Nix flakes][flakes] by setting:

```nix
nix.settings.experimental-features = [ "nix-command" "flakes" ];
```

## Build

To build, simply clone and build with Nix:

```bash
git clone https://github.com/b4er/recalc.git && cd recalc
nix build
```

There should now be a symlink to the build outputs `./result`.

<!-- References: -->

  [nix]: https://nixos.org
  [flakes]: https://nixos.wiki/wiki/Flakes
