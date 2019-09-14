{ nixpkgs ? ./nixpkgs.nix }:
with import nixpkgs {};
haskellPackages.developPackage {
  name = "CanonGame";

  root = ./.;
}
