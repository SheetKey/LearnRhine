{
  description = "A Haskell project template.";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";

    rhine-unstable = {
      url = "github:turion/rhine";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, flake-utils, rhine-unstable }: 
    flake-utils.lib.eachDefaultSystem (system:
      let

        pkgs = nixpkgs.legacyPackages.${system};

        haskellPackages = pkgs.haskellPackages;

        packageName = "rhine-sdl";

        jailbreakUnbreak = pkg:
          pkgs.haskell.lib.doJailbreak (pkg.overrideAttrs (_: { meta = { }; }));

      in {
        packages.${packageName} = # (ref:haskell-package-def)
          haskellPackages.callCabal2nix packageName self rec {
            # Dependency overrides go here
            # Ex with gi-gtk-declarative:
            # If version is broken then:
            # gi-gtk-declarative = jailbreakUnbreak haskellPackages.gi-gtk-declarative;
            # or if tests failing: 
            # gi-gtk-declarative = pkgs.haskell.lib.dontCheck haskellPackages.gi-gtk-declarative;

            rhine = haskellPackages.callCabal2nix "rhine" (rhine-unstable + "/rhine") { };
          };

        defaultPackage = self.packages.${system}.${packageName};

        devShell = pkgs.mkShell {
          buildInputs = with pkgs; [
            ghc
            cabal-install
            haskell-language-server
            haskellPackages.implicit-hie
            cabal2nix

            ## Rhine
            #haskellPackages.rhine
            #haskellPackages.vector-sized

            ## SLD2
            #haskellPackages.sdl2
            #haskellPackages.sdl2-image
            #haskellPackages.sdl2-ttf
            #SDL2
            #pkg-config

            ## Random
            #haskellPackages.random

            ## Generic lens
            #haskellPackages.generic-lens

            ## STM
            #haskellPackages.stm_2_5_1_0
          ];
          inputsFrom = builtins.attrValues self.packages.${system};
        };
      }
    );
}
