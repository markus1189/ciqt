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
        ciqt = (final.haskellPackages.callCabal2nix "ciqt" ./. { }).overrideAttrs (oldAttrs: {
          postInstall = ''
            ${oldAttrs.postInstall or ""}

            # Generate shell completion scripts
            mkdir -p $out/share/bash-completion/completions
            mkdir -p $out/share/zsh/site-functions
            mkdir -p $out/share/fish/vendor_completions.d

            $out/bin/ciqt --bash-completion-script $out/bin/ciqt > $out/share/bash-completion/completions/ciqt
            $out/bin/ciqt --zsh-completion-script $out/bin/ciqt > $out/share/zsh/site-functions/_ciqt
            $out/bin/ciqt --fish-completion-script $out/bin/ciqt > $out/share/fish/vendor_completions.d/ciqt.fish
          '';
        });
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
