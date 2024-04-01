{
  description = "fitness trakcer backend";

  inputs = {
      nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
      flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, flake-utils, ... }@inputs:
    flake-utils.lib.eachSystem ["x86_64-linux"] (system:
    let
      nixpkgs = inputs.nixpkgs.legacyPackages.${system};
      trivial = nixpkgs.lib.trivial;
      haskell-lib = nixpkgs.haskell.lib;
      backend-project = devTools:
       let addBuildTools = (trivial.flip haskell-lib.addBuildTools) devTools;
       in nixpkgs.haskellPackages.developPackage {
         root = ./.; 
         name = "fitness-server-backend";
         returnShellEnv = !(devTools == [ ]);
         modifier = (trivial.flip trivial.pipe) [
           addBuildTools
           haskell-lib.dontHaddock
           haskell-lib.enableStaticLibraries
           haskell-lib.justStaticExecutables
           haskell-lib.disableLibraryProfiling
           haskell-lib.disableExecutableProfiling
         ];
       };
    in
    {
      packages.default = backend-project [ ];
      hydraJobs = { 
        inherit (self) packages; 
        runCommandHook = {
          triggerCI = pkgs.writeScript "action" ''
            #!${nixpkgs.runtimeShell}
            ${nixpkgs.jq}/bin/jq . "$HYDRA_JSON"
          '';
        };
      };
  });
}
