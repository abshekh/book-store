{
  nixConfig = {
    # This sets the flake to use the IOG nix cache.
    # Nix should ask for permission before using it,
    # but remove it here if you do not want it to.
    extra-trusted-substituters = [ "https://cache.iog.io" ];
    extra-trusted-public-keys = [ "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" ];
    allow-import-from-derivation = "true";
  };
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    haskellNix.url = "github:input-output-hk/haskell.nix";
  };
  outputs =
    inputs@{ nixpkgs, flake-parts, haskellNix, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = nixpkgs.lib.systems.flakeExposed;
      perSystem = { system, pkgs, devShells, ... }:
        let
          start-postgres-server =
            (pkgs.writeShellApplication {
              name = "psql-start";
              text = ''
                set -m

                if [ ! -d "$PGDATA" ]; then
                    mkdir "$PGDATA"
                    initdb --no-locale --encoding=UTF-8
                    postgres -D "$PGDATA" &
                    sleep 10   # wait for postgres to start
                    createuser postgres --createdb
                    psql -d postgres -AXqtc "create database \"testdb\""
                    psql -d postgres -AXqtc "create user bilbo with password 'baggins'"
                    psql -d postgres -AXqtc "grant all on database \"testdb\" to bilbo"
                    psql -d testdb -U bilbo < "$PGDATA/../testdb.sql"
                else
                    postgres -D "$PGDATA" &
                fi

                fg %1
              '';
            });
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
                # postgresql_12
                # start-postgres-server
              ];
            };
          };
        };
    };
}
