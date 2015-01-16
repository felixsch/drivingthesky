{ haskellPackages ? (import <nixpkgs> {}).haskellPackages }:
let
  inherit (haskellPackages) cabal cabalInstall Yampa lens
                            OpenGL GLUtil GLFWB vector;
in cabal.mkDerivation (self: {
  pname = "drivingthesky";
  version = "1.0.0";
  src = ./.;
  buildDepends = [
    Yampa
    lens
    OpenGL
    GLUtil
    GLFWB
    vector
    lens
  ];
  buildTools = [ cabalInstall ];
  enableSplitObjs = false;
})
