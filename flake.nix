{
  description = "manage info.yaml";

  inputs = {
    nixpkgs.url      = "github:nixos/nixpkgs/be44bf67"; # nixos-22.05 2022-10-15
    build-utils.url  = "github:sixears/flake-build-utils/r1.0.0.11";

    base1.url        = "github:sixears/base1/r0.0.9.20";
    parsec-plus.url  = "github:sixears/parsec-plus/r1.1.1.16";
    parser-plus.url  = "github:sixears/parser-plus/r1.0.7.12";
    textual-plus.url = "github:sixears/textual-plus/r1.0.2.15";
  };

  outputs = { self, nixpkgs, build-utils
            , base1, parsec-plus, parser-plus, textual-plus }:
    build-utils.lib.hOutputs self nixpkgs "optparse-plus" {
      deps = {
        inherit base1 parsec-plus parser-plus textual-plus;
      };
      ghc = p: p.ghc8107; # for tfmt
    };
}
