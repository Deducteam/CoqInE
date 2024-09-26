{
  description = "Coq to dedukti exporter";
  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable";
  };

  outputs = { self, nixpkgs }: let
    system = "x86_64-linux";
    pkgs = import nixpkgs { inherit system; };
    ocamlPackages = pkgs.ocaml-ng.ocamlPackages_4_06;
    ocamlVersion = ocamlPackages.ocaml.version;
    coq = pkgs.coq.override {
      version = "8.8.2";
      coq-version = "8.8.2";
      customOCamlPackages = ocamlPackages;
    };
    coqPackages = pkgs.mkCoqPackages coq;
    pkg-vodk = coqPackages.callPackage ./vodk.nix {
      inherit coq;
      inherit (ocamlPackages) menhir ocamlbuild findlib;
    };
    dedukti = pkgs.ocamlPackages.dedukti;
    dk = "${dedukti}/bin/dk";

    extraction-context = {
      nativeBuildInputs = [
        pkg-vodk
        coq
        ocamlPackages.ocaml
        pkgs.gnumake
        pkgs.gcc
      ];

      OCAMLPATH = "${ocamlPackages.ocaml}/lib/ocaml/${ocamlVersion}/site-lib:${ocamlPackages.camlp5}/lib/ocaml/${ocamlVersion}/site-lib";
      DKCHECK = "${dk} check";
      DKDEP = "${dk} dep";
    };
    
    tests = pkgs.runCommandLocal "coqine-tests" (extraction-context // { src = ./.; }) ''
      cp -r $src $out
      chmod -R 700 $out
      cd $out
      make test
    '';
  in {
    devShells.${system} = {
      extraction = pkgs.mkShell extraction-context;
    };
    packages.${system} = {
      vokda = pkg-vodk;
      default = pkg-vodk;
    };
    checks.${system} = {
      inherit tests;
    };
  };
}
