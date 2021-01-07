
<!-- README.md is generated from README.Rmd. Please edit that file -->

# LittleAndSu

<!-- badges: start -->

<!-- badges: end -->

The LittleAndSu package implements the Little and Su algorithm first
presented in the chapter “Item nonresponse in panel surveys” in *Panel
Surveys* (1989). This algorithm imputes missing values in longitudinal
datasets: imputing the most useful values when there is a strong row and
column correlational structure. This package also includes two
modifications of the algorithm — one by Watson and Starick in 2011 and
the other by Watson and Li in 2016.

## Installation

You can install LittleAndSu from Github by running the following two
lines

``` r
if(!require("devtools")) install.packages("devtools")
devtools::install_github("julianbernado/LittleAndSu")
```
