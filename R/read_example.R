#' Get path to bulletchartr example files
#'
#' bulletchartr comes bundled with a number of sample files in its `inst/extdata`
#' directory. This function make them easy to access. Taken from 'readr' package.
#'
#' @param path Name of file. If `NULL`, the example files will be listed.
#' @export
#' @examples
#' read_example()
#' read_example("Indicators_Targets.xlsx")

read_example <- function(path = NULL) {
  if (is.null(path)) {
    dir(system.file("extdata", package = "bulletchartr"))
  } else {
    system.file("extdata", path, package = "bulletchartr", mustWork = TRUE)
  }
}
