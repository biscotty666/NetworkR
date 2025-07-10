{
  description = "A basic flake with a shell";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/25.05";
    systems.url = "github:nix-systems/default";
    flake-utils = {
      url = "github:numtide/flake-utils";
      inputs.systems.follows = "systems";
    };
  };

  outputs =
    { nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        #pkgs = nixpkgs.legacyPackages.${system};
      pkgs = import nixpkgs {
        inherit system;
        config.allowUnfree = true;
      };
      in
      {
        devShells.default = pkgs.mkShell {
          nativeBuildInputs = [ pkgs.bashInteractive ];
          buildInputs = with pkgs; [
            R
            quarto
            chromium
            pandoc
            texlive.combined.scheme-full
            rstudio
            radianWrapper
            (with rPackages; [
              quarto
              languageserver
              httpgd
              lintr
              igraph
              network
              intergraph
              reshape
              sna
              egor
              networkDynamic
              tsna
              pagedown
              e1071
              tidyverse
              ggfortify
              ggnetwork
              networkD3
              ndtv
              janitor
              gt
              gtsummary
              lobstr
              memoise
              png
              R6
              GGally
              dbplyr
              RPostgres
              Rcpp
              sessioninfo
              sloop
              testthat
              zeallot
              RSQLite
              bookdown
            ])
          ];
          shellHook = ''
          '';
        };
      }
    );
}
