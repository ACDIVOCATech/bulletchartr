# internal functions ------------------------------------------------------

#' @title Field calculator for regular scale
#' @description internal function for calculating the extra fields needed for bullet charts
#' @param file_name path of Excel file
#' @param sheet_name specify which sheet in Excel file
#' @param dataframe specify R dataframe input
#' @param indicator_name specify the name of the column that has your indicator/KPI names
#' @param info extra info for indicators (currency, percentage, symbol, etc.)
#' @param current current value of indicator
#' @param low column with value for "low"
#' @param medium column with value for "medium"
#' @param high column with value for "high"
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
#' @importFrom forcats as_factor

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
           Target = !!tar,
           info = !!inf
    )

  ## filter NO Target indicators
  if(remove_no_targets == TRUE) {
    ammended_data <- ammended_data %>%
      filter(!is.na(Target), Target != 0)
  } else {
    ammended_data
  }

  ammended_data <- ammended_data %>%
  ## Protect against values greater than what's set at "great"
    mutate(Current = dplyr::if_else(Current > High, High, Current)) %>%
  ## if target value Higher than "High", move "High" to "Target" value
  ## high should not be higher than target ??
    #mutate(High = if_else(target > High, target, High))
    mutate(tarhigh = dplyr::if_else(Target > High, TRUE, FALSE))

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
    select(-tarhigh) %>%
    tidyr::pivot_longer(-c(indicator_name, info),
                 names_to = "allvals",
                 values_to = "vals") %>%
    dplyr::mutate(allvals = forcats::as_factor(allvals))

  ## Variable info text
  ## relevel qualitative labels so show up in order on legend
  ## ammended_data$allvals %>% levels()
  ammended_data <- ammended_data %>%
   mutate(varinfo = glue("{indicator_name}: {info}")) %>%
   mutate(allvals = forcats::fct_relevel(allvals,
                                         c("Low", "Medium", "High", "Current", "Target")))

  # ammended_data <- ammended_data %>%
  #   mutate(allvals = forcats::fct_relevel(allvals, c("Current", "High", "Medium", "Low")))

  return(ammended_data)
}
