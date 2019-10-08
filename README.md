
<!-- README.md is generated from README.Rmd. Please edit that file -->

# bulletchartr

<!-- badges: start -->

[![Codecov test
coverage](https://codecov.io/gh/ACDIVOCATech/bulletchartr/branch/master/graph/badge.svg)](https://codecov.io/gh/ACDIVOCATech/bulletchartr?branch=master)
[![Travis-CI Build
Status](https://travis-ci.org/ACDIVOCATech/bulletchartr.svg?branch=master)](https://travis-ci.org/ACDIVOCATech/bulletchartr)
<!-- badges: end -->

## What is a bullet chart?

The **bullet chart** was invented by [Stephen
Few](https://www.perceptualedge.com/articles/misc/Bullet_Graph_Design_Spec.pIndicatorData),
for the purpose of showing tons of info in a condensed form in KPIs.

This type of graph is a variation on a typical bar graph with a thick
line presenting an important point for that indicator (benchmark,
performance target, etc.) and other bars in the background that can
signify different levels of performance (low-high, bad-good, etc.). The
bullet chart makes it very easy to compare between related measures
(e.g. present status versus status at similar time in the past).

The output of the `bullet_chart()` function most closely resembles
Stephen Few’s design:

``` r
bullet_chart(file_name = "data/Indicators_Targets_ext.xlsx")
```

<img src="man/figures/README-bulletchart-1.png" width="100%" />

The single black bar represents the current value of the indicator while
the different hue columns represent last week’s value (darker hue) and
last year’s value (lighter hue). The bar for each Indicator show the
progression along the horizontal-axis presenting the percentage of the
yearly target completed. This axis also shows the percent of the year
gone by with the vertical line indicating what exact percentage “Today”
is, along this percentage.

As you can see, the bars show the progression along the horizontal-axis
presenting the percentage of the yearly target completed. Also, along
this axis is the percent of the year gone by with a vertical line
indicating what exact percentage **“Today”** is along this percentage.
It is necessary to use percentages as we have multiple indicators of
varying units/parameters for each project\!

The different grey colored bars represent the values of the indicator at
“Last Week” and “Last Year”. The grey scaled bars can represent any
qualitative ranges such as “bad - good - excellent” or “disabled -
repairing - fixed”, etc. In the near future we will look to expand the
capabilities of this package to allow users to specify these qualitative
ranges to fit their needs.

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

(picture of all 4 types)

## Future direction

Currently this package is geared more toward non-R using M\&E people
(therefore, the Excel file input alongside a dataframe input), however
as we develop this package further we want to go towards being able to
make the `bullet_chart` functions more customizable for general use
cases.
