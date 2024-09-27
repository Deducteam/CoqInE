{ lib, mkCoqDerivation, coq, menhir, ocamlbuild, findlib }:

mkCoqDerivation {
  pname = "CoqInE";
  owner = "Deducteam";
  domain = "github.com";

  defaultVersion = ./.;
  mlPlugin = true;
  buildInputs = [ menhir ocamlbuild findlib ];
}
