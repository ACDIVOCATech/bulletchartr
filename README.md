---
title: "Intro to bulletchartr"
author: "Ryo Nakagawara & Amit Kohli"
output: 
  html_document:
    keep_md: true
        
---




## What is a bullet chart?

The __bullet chart__ was invented by [Stephen Few](https://www.perceptualedge.com/articles/misc/Bullet_Graph_Design_Spec.pdf), for the purpose of showing tons of info in a condensed form in KPIs.

This type of graph is a variation on a typical bar graph with a thick line presenting an important point for that indicator (benchmark, performance target, etc.) and other bars in the background that can signify different levels of performance (low-high, bad-good, etc.). The bullet chart makes it very easy to compare between related measures (e.g. present status versus status at similar time in the past).

The output of the `bullet_chart()` function most closely resembles Stephen Few's design: 


```r
bullet_chart(file_name = "data/Indicators_Targets_ext.xlsx")
```

![](README_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

The single black bar represents the current value of the indicator while the different hue columns represent last week's value (darker hue) and last year's value (lighter hue). The bar for each Indicator show the progression along the horizontal-axis presenting the percentage of the yearly target completed. This axis also shows the percent of the year gone by with the vertical line indicating what exact percentage "Today" is, along this percentage. 

As you can see, the bars show the progression along the horizontal-axis presenting the percentage of the yearly target completed. Also, along this axis is the percent of the year gone by with a vertical line indicating what exact percentage __"Today"__ is along this percentage. It is necessary to use percentages as we have multiple indicators of varying units/parameters for each project! 

The different grey colored bars represent the values of the indicator at "Last Week" and "Last Year". The greyscaled bars can represent any qualitative rranges such as "bad - good - excellent" or ____ . In the near future we will look to expand the capabilities of this package to allow users to specify these qualitative ranges to fit their needs. 

This package, `bulletchartr` is based on visualizing M&E deliverables or "Indicators", however, it can be handy for anyone that depends on monitoring Key Performance Indicators (KPIs) or needs to track progress against different targets. 

To use the functions included in this package, one needs to provide an Excel (.xlsx) file containing columns with names that correspond to the following: `indicator_name`, `actual`, `actual_lastweek`, `actual_lastyear`, and `target`. See below for an example of what this kind of data set might look like: 


```
#> Observations: 6
#> Variables: 5
#> $ indicator_name  <chr> "Ind 04", "Ind 05", "Ind 07", "Ind 11", "Ind 1...
#> $ actual          <dbl> 3, 437, 20, 44, 1, 10000
#> $ actual_lastweek <dbl> 3, 420, 18, 20, 1, 10000
#> $ actual_lastyear <dbl> 3, 50, 20, 2000, 1, 10000
#> $ target          <dbl> 14, 81, 21, 10327, 5, 20000
```


The following sections will describe these variables, as well as the extra variables calculated within the function in more detail. In later versions we hope to streamline this process to make it easier to use. 

* `indicator_name`: the name of the indicator or KPI that you are measuring
* `actual`: the value of the indicator at the current time of viewing ("Today")
* `actual_lastweek`: Last week's value of the indicator
* `actual_lastyear`: Last year's value of the indicator
* `target`: the target value for the indicator (used to calculate the percent variables)

With some tidy eval magic you can provide a Excel file with different column names as long as you specify which corresponds to the column names listed above:

<<<<IMAGE OF EXCEL FILE WITH DIFFERENT COLUMN NAMES>>>


```r

bullet_chart(file_name = "data/test.xlsx",
             indicator_name = "Indicators",
             actual = "act",
             actual_lastweek = "WEEKS",
             actual_lastyear = "YEArz",
             target = "MYGOUL")
```

![](README_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

As you can see instead of the proper column names we have some with names like "WEEKS" or "YEArz". By specifying that they are ... we can ensure that the function understands that those are the correct columns for 


Moving on to the variables that are calculated internally by the function... 
The percentages along the horizontal axis are calculated by: 

* `Perc`: Value of indicator as percent of yearly taget and percent of the year at the current time
* `PercWeek`: Last week's value of the indicator as percent of yearly target and percent of the year
* `PercYear`: Last year's value of the indicator as percent of yearly target and percent of the year

`BehindBy` is calculated by: `Perc - PercentTime` and shows how far behind the current value of the indicator is to the target value for the current time and shows up as the text above each bar: 

* "OK!": Shows that the current value of the indicator meets the target value for the current time
* "Need ___ more": Shows exactly how much more of the indicator is needed to reach the target value for the current time


## bullet_chart() functions!

Along with the `bullet_chart()` function that you saw above we also have `bullet_chart2()`.
This is similar to the standard bullet chart but uses different thicknesses for the bars as the benchmarks for previous time points (last week and last year) to further
accentuate the difference graphically.


```r
bullet_chart_wide(file_name = "data/Indicators_Targets_full.xlsx")
```

![](README_files/figure-html/unnamed-chunk-5-1.png)<!-- -->


`bullet_chart_symbols()` shows a version with different symbols representing the indicator value for 
last week (diamond) and last year (circle).


```r
bullet_chart_symbols(file_name = "data/Indicators_Targets_full.xlsx")
```

![](README_files/figure-html/unnamed-chunk-6-1.png)<!-- -->


`bullet_chart_vline()` provides a version with a single colored bar representing the current value
for the indicator along with a black vertical line representing the indicator value at this time
last year.


```r
bullet_chart_vline(file_name = "data/Indicators_Targets_full.xlsx")
```

![](README_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

## Different calendars

The vertical line showing `Today` can be customized depending on whehter you are measuring by a fiscal year, a calendar year, or a custom date. `Today` shown on a Fiscal Year calendar (considering that today's physical date is 2018-03-14.) is show below:


```r
bullet_chart_symbols(file_name = "data/Indicators_Targets_full.xlsx", cal_type = "fis")
```

![](README_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

or for a calendar year:


```r
bullet_chart_symbols(file_name = "data/Indicators_Targets_ext.xlsx", cal_type = "cal")
```

![](README_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

or using a custom date that you can feed directly into the plotting function:


```r
bullet_chart_symbols(file_name = "data/Indicators_Targets_ext.xlsx", cal_type = "2018/02/15")
```

![](README_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

By doing this the function will automatically calculate your progress and targets according to the calendar type that you specified.

## `small` version

If you want to see a small version of your "Plot" panel just specify `small` = "yes". This will allow you to quickly check the the entire plot without having to enlarge it over and over again in the pop-up window.



```r

bullet_chart_wide(file_name = "data/Indicators_Targets.xlsx", small = "yes")
```

![](README_files/figure-html/unnamed-chunk-11-1.png)<!-- -->



## Future Direction

Currently this package is geared more toward non-R using M&E people (therefore, the Excel file input), however as we develop this package further we want to go towards being able to take different file inputs and make the `bullet_chart` functions more customizable for general use cases. 
