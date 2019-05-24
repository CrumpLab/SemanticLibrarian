# Source Code

This github repository [https://github.com/CrumpLab/SemanticLibrarian](https://github.com/CrumpLab/SemanticLibrarian) contains the source code for the Semantic Librarian Shiny App, which should allow the app to be run locally.

## Instructions

1. Install R and R-studio
2. Download the SemanticLibrarian folder to your desktop
  - **Important**: if you download this repo from the website, it will not automatically download the  `allData.RData` (approximately 750MB) in the `allData` folder. This large file is hosted on git lfs (large file-storage). You can separately download this file here [https://github.com/CrumpLab/SemanticLibrarian/blob/master/allData/allData.RData](https://github.com/CrumpLab/SemanticLibrarian/blob/master/allData/allData.RData), by clicking the download button. After this file is downloaded, replace the old `allData.RData` file in the allData folder with the new one.
3. Install the Shiny package
4. Package dependencies. All of the R packages used for this Shiny app are in the Packrat folder. You may be able to run this app without first installing those packages. If not, use the list in the packrat folder to install all of the necessary packages.

## Running the app

1. Open the .Rproj file to load this R project into R-studio. 
2. The Shiny app files are contained in global.R, server.R, and ui.R. Opening any of those files in R studio should allow you to view the `run app` button in the text editor. Press the `run app` button to run the app.

## Contribute

Please use the issues tab to discuss feature requests, bugs, etc.
