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
            inkscape
            (with rPackages; [
              CAinterprTools
              FactoMineR
              GGally
              NLP
              NbClust
              R6
              RColorBrewer
              RPostgres
              RSQLite
              Rcpp
              SnowballC
              bookdown
              car
              dbplyr
              e1071
              egor
              ergm
              factoextra
              ggfortify
              ggnetwork
              gplots
              gt
              gtsummary
              httpgd
              igraph
              intergraph
              janitor
              languageserver
              ldatuning
              lintr
              lobstr
              memoise
              ndtv
              network
              networkD3
              networkDynamic
              pagedown
              plot3D
              png
              quarto
              reshape
              sessioninfo
              sloop
              sna
              svglite
              testthat
              tidyverse
              tm
              tnet
              topicmodels
              tsna
              zeallot
            ])
          ];
          shellHook = ''
          '';
        };
      }
    );
}
