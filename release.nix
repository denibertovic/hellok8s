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
    ${pkgs.dockerTools.shadowSetup}

    exec pid1 "$@"
  '';

  config = {
    packageOverrides = pkgs: rec {

       debianFromDockerHub = pkgs.dockerTools.pullImage {
        imageName = "debian";
        imageDigest = "sha256:f576b8067b77ff85c70725c976b7b6cde960898e2f19b9abab3fb148407614e2";
        sha256 = "sha256:164x4gzxyg6sfapda3bas33x4q307sky15mk49mpdf4glf05xir0";
        finalImageTag = "bullseye-slim";
        finalImageName = "debian";
      };

      hellok8s-docker-image = pkgs.dockerTools.buildImage {
        name = "denibertovic/hellok8s";
        tag = "latest";
        # tag = "${haskellPackages.hellok8s.version}";
        # We can remove some of these packages if we don't end up needing them
        # But I like having some utilities installed
        fromImage =  debianFromDockerHub;
        contents = [ pkgs.bash
                     pkgs.coreutils
                     pkgs.which
                     (pkgs.haskell.lib.justStaticExecutables haskellPackages.hellok8s)
                     (pkgs.haskell.lib.justStaticExecutables haskellPackages.pid1)
                   ];
        config = {
           Entrypoint = [ docker-entrypoint ];
           Cmd = [ "${pkgs.haskell.lib.justStaticExecutables haskellPackages.hellok8s}/bin/hellok8s" ];
           # TODO: bake in keys and configs as well
           WorkingDir = "/opt/app";
           User = "9001";
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
