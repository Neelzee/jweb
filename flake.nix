{
  inputs = {
    nixpkgs.url = "github:cachix/devenv-nixpkgs/rolling";
    systems.url = "github:nix-systems/default";
    devenv.url = "github:cachix/devenv";
    devenv.inputs.nixpkgs.follows = "nixpkgs";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
    purescript-overlay.url = "github:thomashoneyman/purescript-overlay";
    purescript-overlay.inputs.nixpkgs.follows = "nixpkgs";
  };

  nixConfig = {
    extra-trusted-public-keys = "devenv.cachix.org-1:w1cLUi8dv3hnoSPGAuibQv+f9TZLr6cv/Hm9XgU50cw=";
    extra-substituters = "https://devenv.cachix.org";
  };

  outputs = inputs@{ self, nixpkgs, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [ inputs.haskell-flake.flakeModule ];

      perSystem = { self', pkgs, lib, ... }:
      let
        psPkgs = pkgs.extend inputs.purescript-overlay.overlays.default;
        tailwindCSS = pkgs.runCommand "jweb-tailwind-css " {
          nativeBuildInputs = [ pkgs.tailwindcss ];
        } ''
          mkdir -p $out lib static
          cp -r ${./lib}/. lib/
          cp ${./static/input.css} static/input.css
          cp ${./tailwind.config.js} tailwind.config.js

          tailwindcss -i static/input.css -o $out/style.css --minify
        '';
        jweb-js = psPkgs.stdenv.mkDerivation {
          name = "jweb-js";
          src = ./.;
          nativeBuildInputs =
          [
            psPkgs.purs
            psPkgs.spago-unstable
            pkgs.git
            pkgs.cacert
            pkgs.esbuild
          ];
          outputHashMode = "recursive";
          outputHashAlgo = "sha256";
          outputHash = "sha256-+ICMatyx9mch3qkBzDuLZbsdQkCJQXi8RVtD6W1Hkh8=";
          buildPhase = ''
            export HOME=$TMPDIR
            export GIT_SSL_CAINFO="${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt"
            spago install
            spago bundle
          '';
          installPhase = ''
            mkdir -p $out
            cp static/main.js $out/main.js
          '';
        };
        staticDir = "$out/share/jweb/static";
      in
      {

        # Typically, you just want a single project named "default". But
        # multiple projects are also possible, each using different GHC version.
        haskellProjects.default = {
          # The base package set representing a specific GHC version.
          # By default, this is pkgs.haskellPackages.
          # You may also create your own. See https://community.flake.parts/haskell-flake/package-set
          # basePackages = pkgs.haskellPackages;

          # Extra package information. See https://community.flake.parts/haskell-flake/dependency
          #
          # Note that local packages are automatically included in `packages`
          # (defined by `defaults.packages` option).
          #
          packages = {
            # aeson.source = "1.5.0.0";      # Override aeson to a custom version from Hackage
            # shower.source = inputs.shower; # Override shower to a custom source path
          };

          settings = {
            jweb = {
              custom = drv: drv.overrideAttrs (old: {
                postInstall = (old.postInstall or "") + ''
                  mkdir -p $out/share/jweb
                  cp -r --no-preserve=mode ${./static} ${staticDir}
                  cp ${tailwindCSS}/style.css ${staticDir}/style.css
                  cp ${jweb-js}/main.js ${staticDir}/main.js
                  cp ${./specification/specification.yaml} ${staticDir}/specification.yaml
                '';
              });
            };
          };

          devShell = {
            # Enabled by default
            # enable = true;

            # Programs you want to make available in the shell.
            # Default programs can be disabled by setting to 'null'
            tools = hp: {
              #fourmolu = hp.fourmolu;
              #ghcid = null;
              inherit (pkgs) tailwindcss;
              inherit (pkgs) esbuild;
              inherit (psPkgs) purs spago-unstable;
              inherit (pkgs) skopeo;
              inherit (pkgs) openapi-generator-cli;
              inherit (pkgs) just;
              inherit (pkgs) nodejs;
              inherit (pkgs) typescript;
            };

            mkShellArgs = {
              packages = with pkgs; [
                zlib
                glibc
                clib
                nodejs
                playwright-driver.browsers
              ];
              shellHook = ''
                export LD_LIBRARY_PATH=${lib.makeLibraryPath [pkgs.zlib]}
                export PLAYWRIGHT_BROWSERS_PATH=${pkgs.playwright-driver.browsers}
                export PLAYWRIGHT_SKIP_BROWSER_DOWNLOAD=1
              '';
            };


            # Check that haskell-language-server works
            # hlsCheck.enable = true; # Requires sandbox to be disabled
          };
        };

        # haskell-flake doesn't set the default package, but you can do it here.
        packages.default = self'.packages.jweb;

        packages.jweb-js = jweb-js;

        packages.docker =
          let
            envVersion = builtins.getEnv "JWEB_VERSION";
            version = if envVersion != "" then envVersion else "0.0.0.0";
          in
          pkgs.dockerTools.buildLayeredImage {
            name = "jweb";
            tag = "latest";

            contents = [
              self'.packages.jweb
              pkgs.cacert
            ];

            config = {
              Cmd = [ "${self'.packages.jweb}/bin/jweb" ];
              ExposedPorts."3000/tcp" = {};
              WorkingDir = "/data";
              Env = [
                "JWEB_STATIC_DIR=${self'.packages.jweb}/share/jweb/static"
                "JWEB_VERSION=${version}"
              ];
            };
          };
      };
    };
}
