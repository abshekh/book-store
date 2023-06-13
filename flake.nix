{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  outputs =
    inputs@{ nixpkgs, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = nixpkgs.lib.systems.flakeExposed;
      perSystem = { system, pkgs, devShells, ... }:
        let
          overlay = final: prev: {
            book-store = final.callCabal2nix "book-store" ./. { };
          };
          myHaskellPackages = pkgs.haskell.packages.ghc927.extend overlay;
        in
        {
          packages.default = myHaskellPackages.book-store;
          devShells = {
            default = myHaskellPackages.shellFor {
              packages = p: [
                p.book-store
              ];
              buildInputs = with myHaskellPackages; [
                cabal-install
                ghcid
                haskell-language-server
                hindent
              ];

              shellHook = ''
                export PATH="${myHaskellPackages.haskell-language-server}/bin:$PATH"
              '';

            };
          };
        };
    };
}
