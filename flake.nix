{
  inputs = {
    nixpkgs.url = "github:cachix/devenv-nixpkgs/rolling";
    systems.url = "github:nix-systems/default";
    devenv.url = "github:cachix/devenv";
    devenv.inputs.nixpkgs.follows = "nixpkgs";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
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
        tailwindCSS = pkgs.runCommand "jweb-tailwind-css " {
          nativeBuildInputs = [ pkgs.tailwindcss ];
        } ''
          mkdir -p $out src static
          cp -r ${./src}/. src/
          cp ${./static/input.css} static/input.css
          cp ${./tailwind.config.js} tailwind.config.js
          tailwindcss -i static/input.css -o $out/style.css --minify
        '';
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
                  cp -r --no-preserve=mode ${./static} $out/share/jweb/static
                  cp ${tailwindCSS}/style.css $out/share/jweb/static/style.css
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
              tailwindcss = pkgs.tailwindcss;
            };

            mkShellArgs = {
              packages = with pkgs; [
                zlib
                glibc
                clib
              ];
              shellHook = ''
                export LD_LIBRARY_PATH=${lib.makeLibraryPath [pkgs.zlib]}
              '';
            };


            # Check that haskell-language-server works
            # hlsCheck.enable = true; # Requires sandbox to be disabled
          };
        };

        # haskell-flake doesn't set the default package, but you can do it here.
        packages.default = self'.packages.jweb;

        packages.docker = pkgs.dockerTools.buildLayeredImage {
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
            ];
          };
        };
      };
    };
}
