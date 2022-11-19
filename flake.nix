{
  description = "manage info.yaml";

  inputs = {
    nixpkgs.url      = "github:nixos/nixpkgs/be44bf67"; # nixos-22.05 2022-10-15
    build-utils.url  = github:sixears/flake-build-utils/r1.0.0.13;

    base1.url        = github:sixears/base1/r0.0.9.31;
    parsec-plus.url  = github:sixears/parsec-plus/r1.1.1.41;
    parser-plus.url  = github:sixears/parser-plus/r1.0.7.25;
    textual-plus.url = github:sixears/textual-plus/r1.0.2.25;
  };

  outputs = { self, nixpkgs, build-utils
            , base1, parsec-plus, parser-plus, textual-plus }:
    build-utils.lib.hOutputs self nixpkgs "optparse-plus" {
      ghc = p: p.ghc8107; # for tfmt
      callPackage = { mkDerivation, lib, mapPkg, system
                    , base, data-textual, extra, lens, nonempty-containers
                    , optparse-applicative, parsec, parsers, terminal-size, text
                    }:
        mkDerivation {
          pname = "optparse-plus";
          version = "1.3.2.38";
          src = ./.;
          libraryHaskellDepends = [
            base data-textual extra lens nonempty-containers
            optparse-applicative parsec parsers terminal-size text
          ] ++ mapPkg [ base1 parsec-plus parser-plus textual-plus ];
          description = "manage info.yaml";
          license = lib.licenses.mit;
        };
    };
}
