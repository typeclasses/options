{ pkgs }:

let
  inherit (pkgs.lib) fold composeExtensions concatMap attrValues;

  combineOverrides = old: fold composeExtensions (old.overrides or (_: _: { }));

  makeTestConfiguration =
    { ghc ? pkgs.haskellPackages, overrides ? new: old: { } }:
    let inherit (pkgs.haskell.lib) dontCheck packageSourceOverrides;
    in (ghc.override (old: {
      overrides = combineOverrides old [
        (packageSourceOverrides { options = ../options; })
        overrides
      ];
    })).options;

  testConfigurations = rec {
    ghc-9-2 = makeTestConfiguration {
      ghc = pkgs.haskell.packages.ghc92;
      overrides = new: old:
        {
          monads-tf = new.callPackage ./haskell/monads-tf.nix { };
        };
    };
    ghc-9-4 = makeTestConfiguration {
      ghc = pkgs.haskell.packages.ghc94;
      overrides = new: old:
        {
          monads-tf = new.callPackage ./haskell/monads-tf.nix { };
        };
    };
    ghc-9-6 = makeTestConfiguration {
      ghc = pkgs.haskell.packages.ghc96;
      overrides = new: old:
        {
          monads-tf = new.callPackage ./haskell/monads-tf.nix { };
        };
    };
    all = pkgs.symlinkJoin {
      name = "options-tests";
      paths = [ ghc-9-2 ghc-9-4 ghc-9-6 ];
    };
  };

in
{

  packages = { inherit testConfigurations; };

  devShells.default = pkgs.mkShell {
    inputsFrom = [
      (makeTestConfiguration {
        overrides = new: old:
          {
            monads-tf = new.callPackage ./haskell/monads-tf.nix { };
          };
      }).env
    ];
    buildInputs = [ pkgs.haskell-language-server pkgs.cabal-install ];
  };

}
