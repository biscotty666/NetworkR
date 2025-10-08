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
      myvscode = pkgs.vscode-with-extensions.override {
        vscodeExtensions = (with pkgs.vscode-extensions; [
          enkia.tokyo-night
          sainnhe.gruvbox-material
          vscodevim.vim
          reditorsupport.r
        ]);};
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
            myvscode
            (with rPackages; [
              CAinterprTools #
              EpiModel
              FactoMineR #
              GGally #
              NLP #
              NbClust #
              RColorBrewer #
              Rglpk
              RSiena
              SnowballC #
              bookdown
              car #
              dbscan
              egor #
              ergm #
              factoextra #
              fastnet
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
              netdiffuseR
              network #
              networkD3 #
              networkDynamic #
              osmdata
              osmextract
              pagedown
              plot3D #
              png
              quarto #
              relevent
              reshape #
              reshape2
              rgrass
              sf
              sfnetworks
              sna #
              spData
              stplanr
              svglite #
              tergm
              tidygraph
              tidyverse #
              tm #
              tmap
              tnet #
              topicmodels #
              tsna
              webshot
            ])
          ];
          shellHook = ''
          '';
        };
      }
    );
}
