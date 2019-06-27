context("bulletchart")

#testthat::test_that("small argument must be TRUE or FALSE",
#                    expect_error(bullet_chart(dataframe = df, small = 2), "small must be T/F"))
load("inst/data/df.Rda")

## Ensure both dataframe and file not provided
testthat::test_that("Only one dataset inputted: dataframe OR file_name - not both", {

  expect_error(bullet_chart(dataframe = df, file_name = "inst/data/Indicators_Targets.xlsx"))

  expect_error(bullet_chart(dataframe = df), NA)

  expect_error(bullet_chart(file_name = "inst/data/Indicators_Targets.xlsx"), NA)

})


## Expect inputs
testthat::test_that("correct arguments used", {

  file_name <- "inst/data/Indicators_Targets.xlsx"
  checkmate::expect_data_frame(df)
  checkmate::expect_character(file_name)

})


## Expect outputs
testthat::test_that("correct outputs", {

  ## when change to static vs. interactive >> needs to change to include dif arg versions
  expect_equal(class(bullet_chart(dataframe = df)), c("girafe", "htmlwidget"))
  expect_equal(class(bullet_chart_symbols(dataframe = df)), c("girafe", "htmlwidget"))
  expect_equal(class(bullet_chart_vline(dataframe = df)), c("girafe", "htmlwidget"))
  expect_equal(class(bullet_chart_wide(dataframe = df)), c("girafe", "htmlwidget"))
})
