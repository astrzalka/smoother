
<!-- README.md is generated from README.Rmd. Please edit that file -->

# smoother

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

The goal of smoother is to make plots of fluorescence profile across
cells with a trend line.

## Installation

You can install the released version of smoother from Github:

``` r
devtools::install_github('astrzalka/smoother')

smoother::run_app()
```

smoother includes example datasets stored in data-raw directory. Example
dataset test3.txt is included in the app itself for easy testing.

Data can be generated from ImageJ using a script file
imagej\_get\_profile.ijm. This script will generate plot profiles of all
present ROI for all channels and save the ROI to a zip file. txt files
can be directly loaded into the smoother app.
