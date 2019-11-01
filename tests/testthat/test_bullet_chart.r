context("bulletchart")

# data ----
## define test data
test_df <- tibble::tibble(
  indicator_name = c("Ind 04", "Ind 05", "Ind 07", "Ind 11", "Ind 17", "Ind 18"),
  actual = c(3, 437, 20, 44, 1, 10000),
  actual_lastweek = c(3, 420, 18, 20, 1, 10000),
  actual_lastyear = c(3, 50, 20, 2000, 1, 10000),
  target = c(14, 81, 21, 10327, 5, 20000)
)

## define empty data
empty_df <- tibble::tibble()

## define 0 target data
zero_df <- tibble::tibble(
  indicator_name = c("Ind 04", "Ind 05", "Ind 07", "Ind 11", "Ind 17", "Ind 18"),
  actual = c(3, 437, 20, 44, 1, 10000),
  actual_lastweek = c(3, 420, 18, 20, 1, 10000),
  actual_lastyear = c(3, 50, 20, 2000, 1, 10000),
  target = c(0, 0, 0, 0, 0, 0)
)

## data for regular scale bulletchart
bcdatatest <- tibble::tibble(
  variable = c("Order Size", "New Customers",
           "Revenue", "Satisfaction"),
  info = c("Count", "US $", "US $ (1000s)", "Likert Scale of 5"),
  target = c(350, 500, 975, 4.5),
  current = c(365, 310, 1050, 4),
  low = c(220, 430, 600, 2.5),
  medium = c(240, 480, 770, 3.25),
  high = c(400, 505, 1100, 5)
)


## test empty ----

testthat::test_that("error out for empty df", {
  expect_error(bullet_chart_wide(dataframe = empty_df))
})

## test all 0 targets ----

testthat::test_that("error out for zero targets df", {
  expect_equal(bullet_chart_wide(dataframe = zero_df), "No Non-Zero Targets!")
})

## Ensure both dataframe and file not provided
testthat::test_that("Only one dataset inputted: dataframe OR file_name - not both", {

  #expect_error(bullet_chart(dataframe = df, file_name = "inst/data/Indicators_Targets.xlsx"))

  expect_error(bullet_chart_wide(dataframe = test_df), NA)

  #expect_error(bullet_chart(file_name = "inst/data/Indicators_Targets.xlsx"), NA)

})


## Expect inputs ----
context("correct arguments")
testthat::test_that("correct arguments used", {

  checkmate::expect_data_frame(test_df)
  #checkmate::expect_character(file_name)

  ## default
  small <- FALSE
  legend <- FALSE
  remove_no_targets <- FALSE
  chart_type <- "static"

  checkmate::expect_logical(small)
  checkmate::expect_logical(legend)
  checkmate::expect_logical(remove_no_targets)
  checkmate::expect_character(chart_type)

  ## tests
  expect_error(bullet_chart(dataframe = bcdatatest,
                            legend = legend), NA)
  expect_error(bullet_chart_symbols(dataframe = test_df,
                                    chart_type = chart_type,
                                    small = small, legend = legend,
                                    remove_no_targets = remove_no_targets), NA)
  expect_error(bullet_chart_vline(dataframe = test_df,
                                  chart_type = chart_type,
                                  small = small, legend = legend,
                                  remove_no_targets = remove_no_targets), NA)
  expect_error(bullet_chart_wide(dataframe = test_df,
                                 chart_type = chart_type,
                                 small = small, legend = legend,
                                 remove_no_targets = remove_no_targets), NA)

  ## tests interactive
  chart_type <- "interactive"
  checkmate::expect_character(chart_type)
  # expect_error(bullet_chart(dataframe = bcdatatest,
  #                           legend = legend,
  #                           remove_no_targets = remove_no_targets,
  #                           chart_type = chart_type), NA)
  expect_error(bullet_chart_symbols(dataframe = test_df,
                                    small = small, legend = legend,
                                    remove_no_targets = remove_no_targets,
                                    chart_type = chart_type), NA)
  expect_error(bullet_chart_vline(dataframe = test_df,
                                  small = small, legend = legend,
                                  remove_no_targets = remove_no_targets,
                                  chart_type = chart_type), NA)
  expect_error(bullet_chart_wide(dataframe = test_df,
                                 small = small, legend = legend,
                                 remove_no_targets = remove_no_targets,
                                 chart_type = chart_type), NA)

  ## non-default
  small <- TRUE
  legend <- TRUE
  remove_no_targets <- TRUE
  chart_type <- "static"

  checkmate::expect_logical(small)
  checkmate::expect_logical(legend)
  checkmate::expect_logical(remove_no_targets)
  checkmate::expect_character(chart_type)

  ## tests static
  expect_error(bullet_chart(dataframe = bcdatatest,
                            legend = legend), NA)
  expect_error(bullet_chart_symbols(dataframe = test_df,
                                    chart_type = chart_type,
                                    small = small, legend = legend,
                                    remove_no_targets = remove_no_targets), NA)
  expect_error(bullet_chart_vline(dataframe = test_df,
                                  chart_type = chart_type,
                                  small = small, legend = legend,
                                  remove_no_targets = remove_no_targets), NA)
  expect_error(bullet_chart_wide(dataframe = test_df,
                                 chart_type = chart_type,
                                 small = small, legend = legend,
                                 remove_no_targets = remove_no_targets), NA)

  ## tests interactive
  chart_type <- "interactive"
  checkmate::expect_character(chart_type)
  # expect_error(bullet_chart(dataframe = bcdatatest,
  #                           legend = legend,
  #                           remove_no_targets = remove_no_targets,
  #                           chart_type = chart_type), NA)
  expect_error(bullet_chart_symbols(dataframe = test_df,
                                    small = small, legend = legend,
                                    remove_no_targets = remove_no_targets,
                                    chart_type = chart_type), NA)
  expect_error(bullet_chart_vline(dataframe = test_df,
                                  small = small, legend = legend,
                                  remove_no_targets = remove_no_targets,
                                  chart_type = chart_type), NA)
  expect_error(bullet_chart_wide(dataframe = test_df,
                                 small = small, legend = legend,
                                 remove_no_targets = remove_no_targets,
                                 chart_type = chart_type), NA)

  ## small TRUE legend FALSE
  small <- TRUE
  legend <- FALSE
  remove_no_targets <- TRUE
  chart_type <- "static"

  checkmate::expect_logical(small)
  checkmate::expect_logical(legend)
  checkmate::expect_logical(remove_no_targets)
  checkmate::expect_character(chart_type)

  ## tests static
  expect_error(bullet_chart(dataframe = bcdatatest,
                            legend = legend), NA)
  expect_error(bullet_chart_symbols(dataframe = test_df,
                                    chart_type = chart_type,
                                    small = small, legend = legend,
                                    remove_no_targets = remove_no_targets), NA)
  expect_error(bullet_chart_vline(dataframe = test_df,
                                  chart_type = chart_type,
                                  small = small, legend = legend,
                                  remove_no_targets = remove_no_targets), NA)
  expect_error(bullet_chart_wide(dataframe = test_df,
                                 chart_type = chart_type,
                                 small = small, legend = legend,
                                 remove_no_targets = remove_no_targets), NA)

  ## tests interactive
  chart_type <- "interactive"
  checkmate::expect_character(chart_type)
  # expect_error(bullet_chart(dataframe = bcdatatest,
  #                           legend = legend,
  #                           remove_no_targets = remove_no_targets,
  #                           chart_type = chart_type), NA)
  expect_error(bullet_chart_symbols(dataframe = test_df,
                                    small = small, legend = legend,
                                    remove_no_targets = remove_no_targets,
                                    chart_type = chart_type), NA)
  expect_error(bullet_chart_vline(dataframe = test_df,
                                  small = small, legend = legend,
                                  remove_no_targets = remove_no_targets,
                                  chart_type = chart_type), NA)
  expect_error(bullet_chart_wide(dataframe = test_df,
                                 small = small, legend = legend,
                                 remove_no_targets = remove_no_targets,
                                 chart_type = chart_type), NA)

  ## small FALSE legend TRUE
  small <- FALSE
  legend <- TRUE
  remove_no_targets <- TRUE
  chart_type <- "static"

  checkmate::expect_logical(small)
  checkmate::expect_logical(legend)
  checkmate::expect_logical(remove_no_targets)
  checkmate::expect_character(chart_type)

  ## tests static
  expect_error(bullet_chart(dataframe = bcdatatest,
                            legend = legend), NA)
  expect_error(bullet_chart_symbols(dataframe = test_df,
                                    chart_type = chart_type,
                                    small = small, legend = legend,
                                    remove_no_targets = remove_no_targets), NA)
  expect_error(bullet_chart_vline(dataframe = test_df,
                                  chart_type = chart_type,
                                  small = small, legend = legend,
                                  remove_no_targets = remove_no_targets), NA)
  expect_error(bullet_chart_wide(dataframe = test_df,
                                 chart_type = chart_type,
                                 small = small, legend = legend,
                                 remove_no_targets = remove_no_targets), NA)

  ## tests interactive
  chart_type <- "interactive"
  checkmate::expect_character(chart_type)
  # expect_error(bullet_chart(dataframe = bcdatatest,
  #                           legend = legend,
  #                           remove_no_targets = remove_no_targets,
  #                           chart_type = chart_type), NA)
  expect_error(bullet_chart_symbols(dataframe = test_df,
                                    small = small, legend = legend,
                                    remove_no_targets = remove_no_targets,
                                    chart_type = chart_type), NA)
  expect_error(bullet_chart_vline(dataframe = test_df,
                                  small = small, legend = legend,
                                  remove_no_targets = remove_no_targets,
                                  chart_type = chart_type), NA)
  expect_error(bullet_chart_wide(dataframe = test_df,
                                 small = small, legend = legend,
                                 remove_no_targets = remove_no_targets,
                                 chart_type = chart_type), NA)

  ## tests custom cal_type
  cal_type <- "2019/02/15"
  chart_type <- "static"
  checkmate::expect_character(cal_type)
  checkmate::expect_character(chart_type)

  ## static
  expect_error(bullet_chart(dataframe = bcdatatest,
                            legend = legend), NA)
  expect_error(bullet_chart_symbols(dataframe = test_df,
                                    cal_type = cal_type,
                                    small = small, legend = legend,
                                    remove_no_targets = remove_no_targets), NA)
  expect_error(bullet_chart_vline(dataframe = test_df,
                                  cal_type = cal_type,
                                  small = small, legend = legend,
                                  remove_no_targets = remove_no_targets), NA)
  expect_error(bullet_chart_wide(dataframe = test_df,
                                 cal_type = cal_type,
                                 small = small, legend = legend,
                                 remove_no_targets = remove_no_targets), NA)

  ## interactive
  chart_type <- "interactive"
  checkmate::expect_character(chart_type)
  # expect_error(bullet_chart(dataframe = bcdatatest,
  #                           cal_type = cal_type,
  #                           legend = legend,
  #                           remove_no_targets = remove_no_targets,
  #                           chart_type = chart_type), NA)
  expect_error(bullet_chart_symbols(dataframe = test_df,
                                    cal_type = cal_type,
                                    small = small, legend = legend,
                                    remove_no_targets = remove_no_targets,
                                    chart_type = chart_type), NA)
  expect_error(bullet_chart_vline(dataframe = test_df,
                                  cal_type = cal_type,
                                  small = small, legend = legend,
                                  remove_no_targets = remove_no_targets,
                                  chart_type = chart_type), NA)
  expect_error(bullet_chart_wide(dataframe = test_df,
                                 cal_type = cal_type,
                                 small = small, legend = legend,
                                 remove_no_targets = remove_no_targets,
                                 chart_type = chart_type), NA)
})

## Expect outputs ----
testthat::test_that("correct outputs", {

  ## static
  # expect_equal(class(bullet_chart(dataframe = bcdatatest)), c("gg", "ggplot"))
  expect_equal(class(bullet_chart_symbols(dataframe = test_df)), c("gg", "ggplot"))
  expect_equal(class(bullet_chart_vline(dataframe = test_df)), c("gg", "ggplot"))
  expect_equal(class(bullet_chart_wide(dataframe = test_df)), c("gg", "ggplot"))

  ## interactive
  # expect_equal(class(bullet_chart(dataframe = bcdatatest,
  #                                 chart_type = "interactive")), c("girafe", "htmlwidget"))
  expect_equal(class(bullet_chart_symbols(dataframe = test_df,
                                          chart_type = "interactive")), c("girafe", "htmlwidget"))
  expect_equal(class(bullet_chart_vline(dataframe = test_df,
                                        chart_type = "interactive")), c("girafe", "htmlwidget"))
  expect_equal(class(bullet_chart_wide(dataframe = test_df,
                                       chart_type = "interactive")), c("girafe", "htmlwidget"))
})


## need to create test for case when EMPTY data
## need to create test for read_xlsx
## need to create test for cal_type == CUSTOM DATE
## need to create test for Target == 0 in ALL Targets
