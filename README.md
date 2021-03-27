# A sub-national real-time epidemiological and vaccination database for the COVID-19 pandemic in Canada

R code and data to reproduce the figures and tables for the manuscript "A sub-national real-time epidemiological and vaccination database for the COVID-19 pandemic in Canada" by Isha Berry, Meghan O’Neill, Shelby L. Sturrock, James E. Wright, Kamal Acharya, Gabrielle Brankston, Vinyas Harish, Kathy Kornas, Nika Maani, Thivya Naganathan, Lindsay Obress, Tanya Rossi, Alison E. Simmons, Matthew Van Camp, Xiao Xie, Ashleigh R. Tuite, Amy L. Greer, David N. Fisman and Jean-Paul R. Soucy.

## Running this code

First, verify the required R packages are installed (see "load packages" at the beginning of the script). Next, run the script `ccodwg-scientific-data.R` from the root directory. This can be achieved by either loading the project file `ccodwg-scientific-data.Rproj` in RStudio or using `setwd()`. All required data has been provided in the `data` directory.

## Notes

This analysis was produced using R 4.0.3 but any modern version of R should work.

## Terms of use

The code in this repository are released under MIT License. The data in this repository are provided under separate terms of use and are detailed below:

* [COVID-19 Canada Open Data Working Group daily dataset](https://github.com/ccodwg/Covid19Canada)
  * [CC BY 4.0](https://creativecommons.org/licenses/by/4.0/)
* [ESRI Canadian health region boundary files](https://resources-covid19canada.hub.arcgis.com/datasets/regionalhealthboundaries-1)
  * This data is provided "as-is" by Esri Canada and may contain errors or omissions.