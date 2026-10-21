{
  description = "anima2D: a self-hosted 2D animation studio for the browser and Apple Pencil";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";

    pre-commit-hooks = {
      url = "github:cachix/git-hooks.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = {
    self,
    nixpkgs,
    flake-utils,
    pre-commit-hooks,
    ...
  }:
    flake-utils.lib.eachSystem ["aarch64-linux" "aarch64-darwin" "x86_64-linux"] (
      system: let
        pkgs = import nixpkgs {inherit system;};
        # nixpkgs' default GHC: the package set the binary cache covers in full.
        hpkgs = pkgs.haskellPackages;
        inherit (pkgs) lib;

        backendSrc = lib.fileset.toSource {
          root = ./.;
          fileset = lib.fileset.unions [./app ./src ./test ./fixtures ./anima2d.cabal ./CHANGELOG.md ./LICENSE];
        };

        backend = pkgs.haskell.lib.overrideCabal (hpkgs.callCabal2nix "anima2d" backendSrc {}) {
          doCheck = true;
          # hspec-discover is a preprocessor found on PATH.
          testToolDepends = [hpkgs.hspec-discover];
          doHaddock = false;
        };

        # The editor: type-checked, linted and specced as part of its build, so a
        # rule broken in TypeScript fails `nix flake check` like one broken in Haskell.
        web = pkgs.buildNpmPackage {
          pname = "anima2d-web";
          version = backend.version;
          # The shared parser fixtures sit beside web/, and its specs read them.
          src = lib.fileset.toSource {
            root = ./.;
            fileset = lib.fileset.unions [
              (lib.fileset.difference ./web (lib.fileset.unions [(lib.fileset.maybeMissing ./web/node_modules) (lib.fileset.maybeMissing ./web/dist)]))
              ./fixtures
            ];
          };
          sourceRoot = "source/web";
          # Refresh with `just npm-hash` after changing web/package-lock.json.
          npmDepsHash = lib.fileContents ./web/npm-deps.hash;
          npmBuildScript = "build";
          doCheck = true;
          checkPhase = ''
            runHook preCheck
            npm run check
            runHook postCheck
          '';
          installPhase = ''
            runHook preInstall
            cp -r dist $out
            runHook postInstall
          '';
        };

        anima2d = pkgs.symlinkJoin {
          name = "anima2d-${backend.version}";
          paths = [backend];
          nativeBuildInputs = [pkgs.makeWrapper];
          postBuild = ''
            wrapProgram $out/bin/anima2d \
              --set-default LANG C.UTF-8 \
              --set-default ANIMA2D_STATIC ${web}
          '';
          meta.mainProgram = "anima2d";
        };
      in {
        packages = {
          default = anima2d;
          inherit anima2d backend web;
        };

        checks = {
          inherit backend web;
          pre-commit-check = pre-commit-hooks.lib.${system}.run {
            src = ./.;
            hooks = {
              fourmolu.enable = true;
              fourmolu.package = hpkgs.fourmolu;
              hlint.enable = true;
              hlint.package = hpkgs.hlint;
              alejandra.enable = true;
            };
          };
        };

        devShells.default = hpkgs.shellFor {
          name = "anima2d-dev-shell";
          packages = _: [backend];
          withHoogle = false;
          nativeBuildInputs = [
            hpkgs.cabal-install
            hpkgs.hspec-discover
            hpkgs.haskell-language-server
            hpkgs.fourmolu
            hpkgs.hlint
            pkgs.nodejs
            pkgs.just
            pkgs.prefetch-npm-deps
            self.checks.${system}.pre-commit-check.enabledPackages
          ];
          shellHook = self.checks.${system}.pre-commit-check.shellHook;
        };

        formatter = pkgs.alejandra;
      }
    );
}
