context("extra_field_calculator")

#testthat::test_that("only one file type inputted",
#                    expect_equal(extra_field_calculator(file_name = "data/Indicators_Targets.xlsx"),
#                                 extra_field_calculator(file_name = "data/Indiacotrs_Targets.xlsx", dataframe = NULL)))




################################################# TESTING


# ## Inputs
# fileName="Indicators_Targets.com"   # NOT excel file
# fileName="data/Indicators_Targets.xlsx"  # excel file
# for_year = 2018 ## Specify Year the analysis represents
# FY = TRUE       ## Is this a fiscal year? (as opposed to calendar year)
# project_start_date <- "2016/03/01"   # as string! %Y/%D/%M
#
# ##
# dataframe <- extra_field_calculator("data/Indicators_Targets.xlsx", for_year = 2018, FY = TRUE, project_start_date = project_start_date)
# df <- extra_field_calculator(fileName, for_year = 2018, FY = TRUE, project_start_date = project_start_date)
