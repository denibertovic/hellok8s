let
  sources = import ./nix/sources.nix;
  # Disable tests for these packages
  dontCheckPackages = [
  ];

  # Jailbreak these packages
  doJailbreakPackages = [
  ];

  # Disable haddocks for these packages
  dontHaddockPackages = [
  ];

  docker-entrypoint = pkgs.writeScript "entrypoint.sh" ''
    #!${pkgs.stdenv.shell}
    # Add local user
    # Either use the LOCAL_USER_ID if passed in at runtime or
    # fallback
    USER_ID=''${LOCAL_USER_ID:-9001}
    APP_DIR=''${APP_DIR:-/opt/app}

    echo "Starting with UID : $USER_ID"
    useradd --shell ${pkgs.runtimeShell} -U -u ''${USER_ID} -o -c "The App User" -m user
    export HOME=/home/user

    # set correct permissions on APP_DIR and subfolders
    chown -R user:user $APP_DIR

    exec pid1 -u user -g user "$@"
  '';

  config = {
    packageOverrides = pkgs: rec {

       debianFromDockerHub = pkgs.dockerTools.pullImage {
        imageName = "debian";
        imageDigest = "sha256:27586f4609433f2f49a9157405b473c62c3cb28a581c413393975b4e8496d0ab";
        sha256 = "sha256-9VhHSzRzW1dWAAXF/7B3l3vwRzGSKBrGziHmBdnVit4=";
        finalImageTag = "bookworm";
        finalImageName = "debian";
      };

      hellok8s-docker-image = pkgs.dockerTools.buildImage {
        name = "denibertovic/hellok8s";
        tag = "latest";
        # tag = "${haskellPackages.hellok8s.version}";
        # We can remove some of these packages if we don't end up needing them
        # But I like having some utilities installed
        created = "now";
        fromImage =  debianFromDockerHub;
        copyToRoot = pkgs.buildEnv {
          name =  "image-root";
          paths = [ pkgs.stdenv
                    pkgs.cacert
                    pkgs.dockerTools.binSh
                    pkgs.ps
                    pkgs.curl
                    pkgs.wget
                    (pkgs.haskell.lib.justStaticExecutables haskellPackages.hellok8s)
                    (pkgs.haskell.lib.justStaticExecutables haskellPackages.pid1)
                  ];
        };
        config = {
           Entrypoint = [ docker-entrypoint ];
           Cmd = [ "${pkgs.haskell.lib.justStaticExecutables haskellPackages.hellok8s}/bin/hellok8s" ];
           # TODO: bake in keys and configs as well
           WorkingDir = "/opt/app";
         };
       };
      haskellPackages =
        let
          generatedOverrides = haskellPackagesNew: haskellPackagesOld:
            let
              toPackage = file: _: {
                name  = builtins.replaceStrings [ ".nix" ] [ "" ] file;

                value = haskellPackagesNew.callPackage (./. + "/nix/${file}") { };
              };

            in
              pkgs.lib.mapAttrs' toPackage (builtins.readDir ./nix);

          makeOverrides =
            function: names: haskellPackagesNew: haskellPackagesOld:
              let
                toPackage = name: {
                  inherit name;

                  value = function haskellPackagesOld.${name};
                };

            in
              builtins.listToAttrs (map toPackage names);

          composeExtensionsList =
            pkgs.lib.fold pkgs.lib.composeExtensions (_: _: {});

          # More exotic overrides go here
          manualOverrides = haskellPackagesNew: haskellPackagesOld: {
          };
        in
          pkgs.haskellPackages.override {
            overrides = composeExtensionsList [
              generatedOverrides
              (makeOverrides pkgs.haskell.lib.dontCheck   dontCheckPackages  )
              (makeOverrides pkgs.haskell.lib.doJailbreak doJailbreakPackages)
              (makeOverrides pkgs.haskell.lib.dontHaddock dontHaddockPackages)
              manualOverrides
            ];
          };
    };
  };
  # This is VERY important as we PIN nixpkgs so we're not dependant on what the
  # global nix-channel is set to on your host system
  pkgs = import sources.nixpkgs { inherit config; };

in
  { hellok8s = pkgs.haskellPackages.hellok8s;
    hellok8s-docker-image = pkgs.hellok8s-docker-image;
  }
