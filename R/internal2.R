# internal functions ------------------------------------------------------

#' @title extra_field_calculator
#' @description internal function for calculating the extra fields needed for bullet charts
#' @param file_name path of Excel file
#' @param sheet_name specify which sheet in Excel file
#' @param dataframe specify R dataframe input
#' @param indicator_name specify the name of the column that has your indicator/KPI names
#' @param info extra info for indicators (currency, percentage, symbol, etc.)
#' @param current current value of indicator
#' @param bad column with value for "bad"
#' @param good column with value for "good"
#' @param great column with value for "great", if "target" exceeds "great" then
#' "great" will be increased to "target" value. If "current" value exceeds "great"
#' then "current" value will be decreased to "great" value.
#' @param target column with value for the "target"
#' @param remove_no_targets Removes indicators without a "target" value specified, default: TRUE
#' @details internal function for calculating the extra fields for the bullet chart
#' @rdname field_calculator
#' @importFrom dplyr mutate %>% case_when filter
#' @importFrom testthat test_that expect_equal expect_true
#' @importFrom readxl read_xlsx
#' @importFrom lubridate year month
#' @importFrom rlang enquo !!
#' @importFrom glue glue

field_calculator <- function(file_name = NULL, sheet_name = "Sheet1",
                             dataframe = NULL,
                             indicator_name = "variable",
                             info = "info",
                             current = "current",
                             low = "low",
                             medium = "medium",
                             high = "high",
                             target = "target",
                             remove_no_targets = TRUE) {


  ## Ensure both dataframe and file not provided
  test_that("Only one dataset inputted: dataframe OR file_name - not both",
            expect_true(is.null(file_name) | is.null(dataframe)))

  ## Read in excel file or dataframe
  if (!is.null(file_name)) {
    ## Read in Excel file:
    ammended_data <- read_xlsx(path = file_name, sheet = sheet_name)

  } else if(!is.null(dataframe)) {
    ## dataframe provided
    ammended_data <- dataframe
  }

  ## If ammended_data is empty, break function and output empty chart
  if (nrow(ammended_data) == 0){
    return("No data available")
  }

  ## Assign field names to this dataset
  ind <- enquo(indicator_name)
  inf <- enquo(info)
  Cur <- enquo(current)
  Low <- enquo(low)
  Medium <- enquo(medium)
  High <- enquo(high)
  tar <- enquo(target)

  ammended_data <- ammended_data %>%
    select(indicator_name = !!ind,
           Current = !!Cur,
           Low = !!Low,
           Medium = !!Medium,
           High = !!High,
           target = !!tar
    )

  ## filter NO Target indicators
  if(remove_no_targets == TRUE) {
    ammended_data <- ammended_data %>%
      filter(!is.na(target), target != 0)
  } else {
    ammended_data
  }

  ammended_data <- ammended_data %>%
  ## Protect against values greater than what's set at "great"
    mutate(Current = dplyr::if_else(Current > High, High, Current)) %>%
  ## if target value Higher than "High", move "High" to "Target" value
  ## high should not be higher than target ??
    #mutate(High = if_else(target > High, target, High))
    mutate(tarhigh = dplyr::if_else(target > High, TRUE, FALSE))

  ## if Target > High then error out with name of columns
  if (any(ammended_data$tarhigh == TRUE)) {
    tarhighvars <- ammended_data %>%
      filter(tarhigh == TRUE) %>%
      pull(indicator_name) %>%
      paste(collapse = ", ")
    tarhighmess <- paste0("The following variables have Targets > High values:", tarhighvars)
    stop(tarhighmess)
  }

  ## reshape
  ammended_data <- ammended_data %>%
    tidyr::pivot_longer(-c(indicator_name, target),
                 names_to = "allvals",
                 values_to = "vals") %>%
    dplyr::mutate(allvals = forcats::as_factor(allvals))

  # Variable info text:
  ammended_data <- ammended_data %>%
    mutate(varinfo = glue("{indicator_name}: {info}"))

  # ammended_data <- ammended_data %>%
  #   mutate(allvals = forcats::fct_relevel(allvals, c("Current", "High", "Medium", "Low")))

  return(ammended_data)
}
