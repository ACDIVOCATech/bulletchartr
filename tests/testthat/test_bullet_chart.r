context("bulletchart")

#testthat::test_that("small argument must be TRUE or FALSE",
#                    expect_error(bullet_chart(dataframe = df, small = 2), "small must be T/F"))
#load("inst/data/df.Rda")

## define test data
test_df <- tibble::tibble(
  indicator_name = c("Ind 04", "Ind 05", "Ind 07", "Ind 11", "Ind 17", "Ind 18"),
  actual = c(3, 437, 20, 44, 1, 10000),
  actual_lastweek = c(3, 420, 18, 20, 1, 10000),
  actual_lastyear = c(3, 50, 20, 2000, 1, 10000),
  target = c(14, 81, 21, 10327, 5, 20000)
)

## Ensure both dataframe and file not provided
testthat::test_that("Only one dataset inputted: dataframe OR file_name - not both", {

  #expect_error(bullet_chart(dataframe = df, file_name = "inst/data/Indicators_Targets.xlsx"))

  expect_error(bullet_chart(dataframe = test_df), NA)

  #expect_error(bullet_chart(file_name = "inst/data/Indicators_Targets.xlsx"), NA)

})


## Expect inputs
# testthat::test_that("correct arguments used", {
#
#   file_name <- "inst/data/Indicators_Targets.xlsx"
#   checkmate::expect_data_frame(df)
#   checkmate::expect_character(file_name)
#
# })


## Expect outputs
testthat::test_that("correct outputs", {

  ## static
  expect_equal(class(bullet_chart(dataframe = test_df)), c("gg", "ggplot"))
  expect_equal(class(bullet_chart_symbols(dataframe = test_df)), c("gg", "ggplot"))
  expect_equal(class(bullet_chart_vline(dataframe = test_df)), c("gg", "ggplot"))
  expect_equal(class(bullet_chart_wide(dataframe = test_df)), c("gg", "ggplot"))

  ## interactive
  expect_equal(class(bullet_chart(dataframe = test_df,
                                  chart_type = "interactive")), c("girafe", "htmlwidget"))
  expect_equal(class(bullet_chart_symbols(dataframe = test_df,
                                          chart_type = "interactive")), c("girafe", "htmlwidget"))
  expect_equal(class(bullet_chart_vline(dataframe = test_df,
                                        chart_type = "interactive")), c("girafe", "htmlwidget"))
  expect_equal(class(bullet_chart_wide(dataframe = test_df,
                                       chart_type = "interactive")), c("girafe", "htmlwidget"))
})


