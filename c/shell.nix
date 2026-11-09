# Build with hTox's Haskell packages: `nix-shell --run 'cabal build'`.
let
  hatter = import ../../htox/nix/hatter.nix;
  inherit (hatter) pkgs;
  hpkgs = (import "${hatter.src}/nix/hpkgs.nix" { inherit pkgs; }).extend
    (pkgs.lib.composeExtensions (import ../../htox/nix/packages.nix pkgs) (self: super: {
      tox-c = self.callCabal2nix "tox-c" ./. { };
    }));
in
hpkgs.shellFor {
  packages = ps: [ ps.tox-c ];
  nativeBuildInputs = [ pkgs.cabal-install ];
}
