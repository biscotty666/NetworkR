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
            grass
            inkscape
            (with rPackages; [
              CAinterprTools #
              FactoMineR #
              GGally #
              NLP #
              NbClust #
              RColorBrewer #
              SnowballC #
              bookdown
              car #
              egor #
              ergm #
              factoextra #
              geodata
              ggfortify
              ggnetwork #
              gplots #
              gt
              gtsummary
              httpgd
              igraph #
              intergraph #
              janitor #
              languageserver
              ldatuning #
              link2GI
              lintr
              maptiles
              memoise
              nabor
              ndtv #
              network #
              networkD3 #
              networkDynamic #
              osmdata
              osmextract
              pagedown
              plot3D #
              png
              quarto #
              reshape #
              rgrass
              sf
              sfnetworks
              sna #
              spData
              stplanr
              svglite #
              tidygraph
              tidyverse #
              tm #
              tmap
              tnet #
              topicmodels #
            ])
          ];
          shellHook = ''
          '';
        };
      }
    );
}
