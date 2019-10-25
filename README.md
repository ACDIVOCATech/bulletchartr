
<!-- README.md is generated from README.Rmd. Please edit that file -->

# bulletchartr

<!-- badges: start -->

[![Codecov test
coverage](https://codecov.io/gh/ACDIVOCATech/bulletchartr/branch/master/graph/badge.svg)](https://codecov.io/gh/ACDIVOCATech/bulletchartr?branch=master)
[![Travis-CI Build
Status](https://travis-ci.org/ACDIVOCATech/bulletchartr.svg?branch=master)](https://travis-ci.org/ACDIVOCATech/bulletchartr)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

This package, `bulletchartr` is based on visualizing M\&E deliverables
or “Indicators”, however, it can be handy for anyone that depends on
monitoring Key Performance Indicators (KPIs) or needs to track progress
against different targets.

## Installation

``` r
# Install the package from GitHub:
# install.packages("devtools")

devtools::install_github("ACDIVOCATech/bulletchartr")
```

The **bullet chart** was invented by [Stephen
Few](https://www.perceptualedge.com/articles/misc/Bullet_Graph_Design_Spec.pIndicatorData),
for the purpose of showing tons of info in a condensed form in KPIs.

The output of the `bullet_chart()` function most closely resembles
Stephen Few’s design:

``` r
## load example data
load(read_example("df_bc.rda"))

bullet_chart(dataframe = df_bc)
```

<img src="man/figures/README-bulletchart-1.png" width="100%" />

The outputs of `bullet_chart_symbols()`, `bullet_chart_wide()`, and
`bullet_chart_vline()` have a different x-axis scale to a regular bullet
chart.

The x-axis represents both the percentage of the yearly target AND the
percentage of the year that has passed. There is a vertical line showing
`TODAY`, which shows at what percentage of the year **and** what
percentage of the target we are at right now. The color inside the bar
is **green** if we are near or past the `TODAY` line, **orange** when
weâ€™re close and **red** when we’re very behind schedule/target.

``` r
bullet_chart_symbols(file_name = read_example("Indicators_Targets_ext.xlsx"))
```

<img src="man/figures/README-unnamed-chunk-1-1.png" width="100%" />

``` r
bullet_chart_wide(file_name = read_example("Indicators_Targets_ext.xlsx"))
```

<img src="man/figures/README-unnamed-chunk-2-1.png" width="100%" />

``` r
bullet_chart_vline(file_name = read_example("Indicators_Targets_ext.xlsx"))
```

<img src="man/figures/README-unnamed-chunk-3-1.png" width="100%" />

Please read the package vignette, “Introduction to bullet charts” for a
more detailed overview\!

## Future direction

Currently this package is geared more toward non-R using M\&E people
(therefore, the Excel file input alongside a dataframe input), however
as we develop this package further we want to go towards being able to
make the `bullet_chart` functions more customizable for general use
cases.
