{
  description = "A Haskell project template.";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }: 
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        haskellPackages = pkgs.haskellPackages;

        packageName = "Generic";

        jailbreakUnbreak = pkg:
          pkgs.haskell.lib.doJailbreak (pkg.overrideAttrs (_: { meta = { }; }));

      in {
        packages.${packageName} = # (ref:haskell-package-def)
          haskellPackages.callCabal2nix packageName self rec {
            # Dependency overrides go here
            # Ex with gi-gtk-declarative:
            # If version is broken then:
            # gi-gtk-declarative = jailbreakUnbreak haskeppPackages.gi-gtk-declarative;
            # or if tests failing: 
            # gi-gtk-declarative = pkgs.haskell.lib.dontCheck haskellPackages.gi-gtk-declarative;
          };

        defaultPackage = self.packages.${system}.${packageName};

        devShell = pkgs.mkShell {
          buildInputs = with pkgs; [
            ghc
            cabal-install
            haskell-language-server
            haskellPackages.implicit-hie
          ];
          inputsFrom = builtins.attrValues self.packages.${system};
        };
      }
    );
}
