{
  description = "A commandline tool to run cloudwatch insight queries";

  outputs = { self, nixpkgs }:
    let
      supportedSystems = [ "x86_64-linux" "x86_64-darwin" ];
      forAllSystems = f:
        nixpkgs.lib.genAttrs supportedSystems (system: f system);
      nixpkgsFor = forAllSystems (system:
        import nixpkgs {
          inherit system;
          overlays = [ self.overlay ];
        });

    in {
      overlay = (final: prev: {
        ciqt = (final.haskellPackages.callCabal2nix "ciqt" ./. { });
      });
      packages = forAllSystems (system: { ciqt = nixpkgsFor.${system}.ciqt; });
      defaultPackage = forAllSystems (system: self.packages.${system}.ciqt);
      apps = forAllSystems (system: {
        ciqt = {
          type = "app";
          program = "${self.packages.${system}.ciqt}/bin/ciqt";
        };
        default = self.apps.${system}.ciqt;
      });
      checks = self.packages;
      devShell = forAllSystems (system:
        let
          haskellPackages = nixpkgsFor.${system}.haskellPackages;
        in
          haskellPackages.shellFor {
          packages = p: [ self.packages.${system}.ciqt ];
          withHoogle = true;
          buildInputs = with haskellPackages; [
            haskell-language-server
            ghcid
            cabal-install
            ormolu
          ];
        });
    };
}
