# bulletchartr 0.3.1

* Added legend entry for "Target" as per [#24](https://github.com/ACDIVOCATech/bulletchartr/issues/24)
* Fixed [#34](https://github.com/ACDIVOCATech/bulletchartr/issues/34) regarding 'Unknown or uninitialized column' warnings
* Reordered items in legend as per [#36](https://github.com/ACDIVOCATech/bulletchartr/issues/36)
* Updated README
* pkgdown website updated
* Update package logo with new target legend changes

# bulletchartr 0.3.0

* `bullet_chart()` function now uses regular scale **only**
* `bullet_chart()` uses 'Low-Medium-High' qualitative labels
* Other bullet chart variants still use time-comparison scale
* Fixed how data is used/stored for package
* Fixed sizing issues to comply with new 'ggiraph' version
* Fixed 'show_text' option for time-comparison variants
* Revamped all vignettes and README to be consistent with all changes
* 'legend' argument defaults to TRUE
* Created pkgdown website
* Fixed "TODAY" label not appearing for `_vline()` function

# bulletchartr 0.2.0

* Added a `NEWS.md` file to track changes to the package.
* Fixed fill legend code.
* Added more tests.
* Change license to MIT.

# bulletchartr 0.1.0

* All bulletcharts now use 'ggiraph', hover mouse on the bar to see how behind/ahead you currently are compared to "Last Week" and "Last Year".
* Legend for colorbar hidden.
* "No Target" text appears above bar if "Target" column is empty/NA.
* Separated internal funs to separate script file .
* "No Non-Zero Targets!" shows as simple text instead of on empty plot.
* Added more tests.

# bulletchartr 0.0.0.990

* All charts use 'ggiraph'.
* Legend for colorbar hidden.
* Added "no target" text.
* increase expand_limits for "no target" text on top row.

# bulletchartr 0.0.0.900

* initial upload
