{
  description = "The Amethyst programming language";

  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = (import nixpkgs { inherit system; });
        hpkgs = pkgs.haskellPackages;

        my-drv = {
          root = ./.;
          withHoogle = false;
          modifier = drv: pkgs.haskell.lib.addBuildTools drv (
            with pkgs;
            with hpkgs;
            [
              bashInteractive
              cabal-install
              haskell-language-server
            ]
          );
        };
      in
      {
        defaultPackage = hpkgs.developPackage my-drv;
        devShell = hpkgs.developPackage (my-drv // {
          returnShellEnv = true;
        });
      });
}
