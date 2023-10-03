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
      amazonkaVer = "2.0";
      haskellPackagesModifier = hsPkgs:
        hsPkgs.override {
          overrides = final: prev: {
            amazonka =
              final.callHackageDirect {
                pkg = "amazonka";
                ver = amazonkaVer;
                sha256 = "sha256-ut71byLgmg9SgZCfIrDbG76LI7Buq+x6F4oHTTuEdHI=";
              } { };
            amazonka-sso = final.callHackageDirect {
              pkg = "amazonka-sso";
              ver = amazonkaVer;
              sha256 = "sha256-+632/wu9Vdii8f5NwFeypXUeUV5b3DgMonUluiwO3F0=";
            } { };
            amazonka-core = final.callHackageDirect {
              pkg = "amazonka-core";
              ver = amazonkaVer;
              sha256 = "sha256-KVTe6IlVDNaot1XuFjbvlUs/jmeoyEfqnDYsb4V1K2g=";
            } { };
            amazonka-sts = final.callHackageDirect {
              pkg = "amazonka-sts";
              ver = amazonkaVer;
              sha256 = "sha256-5eRQ5zH5gsoiJZMwq4eepUyDBHzrIZFOPA6vKOCSuHQ=";
            } { };
            amazonka-cloudwatch-logs = final.callHackageDirect {
              pkg = "amazonka-cloudwatch-logs";
              ver = amazonkaVer;
              sha256 = "sha256-BDY6VQdx8Z3b6TdWzIxryNv3eZTUGudGhEKRD2LAECU=";

            } { };
            amazonka-test = final.callHackageDirect {
              pkg = "amazonka-test";
              ver = amazonkaVer;
              sha256 = "sha256-lFXvtzj4p3aqMpRMyCz32jpkET3tE7BaUf6+2iwQ/ok=";

            } { };

            crypton = assert prev.crypton.version != "0.33"; final.callHackageDirect {
              pkg = "crypton";
              ver = "0.33";
              sha256 = "sha256-bq1ypwOhYC8OR5XDWDj0u4+gTygxcwnPL+IffUWvlhg=";
            } { };
          };
        };

    in {
      overlay = (final: prev: {
        ciqt =
          (haskellPackagesModifier final.haskellPackages).callCabal2nix "ciqt"
          ./. { };
      });
      packages = forAllSystems (system: { ciqt = nixpkgsFor.${system}.ciqt; });
      defaultPackage = forAllSystems (system: self.packages.${system}.ciqt);
      checks = self.packages;
      devShell = forAllSystems (system:
        let
          haskellPackages =
            haskellPackagesModifier nixpkgsFor.${system}.haskellPackages;
        in
          haskellPackages.shellFor {
          packages = p: [ self.packages.${system}.ciqt ];
          withHoogle = true;
          buildInputs = with haskellPackages; [
            haskell-language-server
            ghcid
            cabal-install
          ];
        });
    };
}
