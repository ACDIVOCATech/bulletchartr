context("bulletchart")

data("df")

#testthat::test_that("small argument must be TRUE or FALSE",
#                    expect_error(bullet_chart(dataframe = df, small = 2), "small must be T/F"))


## Ensure both dataframe and file not provided
testthat::test_that("Only one dataset inputted: dataframe OR file_name - not both",
                    testthat::expect_error(

                      bulletchart(dataframe = df, file_name = "data/Indicators_Targets.xlsx")

                      )
                    )
