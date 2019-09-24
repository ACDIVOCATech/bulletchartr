# internal functions ------------------------------------------------------

#' @title extra_field_calculator
#' @description internal function for calculating the extra fields needed for bullet charts
#' @param file_name path of Excel file
#' @param sheet_name specify which sheet in Excel file
#' @param dataframe specify R dataframe input
#' @param indicator_name specify the name of the column that has your indicator/KPI names
#' @param actual specify the name of the column that has the current value of your indicators/KPIs
#' @param actual_lastweek specify the name of the column that has the indicator/KPI value from the previous week
#' @param actual_lastyear specify the name of the column that has the indicator/KPI value from the previous year
#' @param target specify the name of the column that has the target value for the indicator/KPI
#' @param for_year specify the year in which the report is being made, Default: year(Sys.Date())
#' @param cal_type define what calendar you are using. Options are "fis" for fiscal year starting
#' @param remove_no_targets remove indicators with Targets == NA or 0, Default: FALSE
#' October 1st, "cal" for calendar year starting January 1st, or enter your own custom date in the
#' format "YYYY/MM/DD", Default: fis
#' @details internal function for calculating the extra fields for the bullet chart
#' @seealso
#'  \code{\link[dplyr]{mutate}}
#'  \code{\link[readxl]{read_excel}}
#' @rdname extra_field_calculator
#' @importFrom dplyr mutate %>% case_when filter
#' @importFrom testthat test_that expect_equal expect_true
#' @importFrom readxl read_xlsx
#' @importFrom lubridate year month
#' @importFrom rlang enquo !!
#' @importFrom glue glue
#' @importFrom stringr str_replace_all

extra_field_calculator <- function(file_name = NULL, sheet_name = "Sheet1",
                                   dataframe = NULL,
                                   indicator_name = "indicator_name",
                                   actual = "actual",
                                   actual_lastweek = "actual_lastweek",
                                   actual_lastyear = "actual_lastyear",
                                   target = "target",
                                   for_year = year(Sys.Date()),
                                   cal_type = "fis", remove_no_targets = FALSE) {


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

  # If ammended_data is empty, break function and output empty chart
  if (nrow(ammended_data) == 0){
    return("No data available")

  }


  ## Assign field names to this dataset
  ind <- enquo(indicator_name)
  a <- enquo(actual)
  al <- enquo(actual_lastweek)
  ay <- enquo(actual_lastyear)
  t <- enquo(target)

  ammended_data <- ammended_data %>%
    select(indicator_name = !!ind,
           actual = !!a,
           actual_lastweek = !!al,
           actual_lastyear = !!ay,
           target = !!t
    )
  # Create percentage variables
  ammended_data <- ammended_data %>%
    mutate(perc = actual / (target + 0.0000000000001) * 100,
           perc_week = actual_lastweek / (target + 0.0000000000001) * 100,
           perc_year = actual_lastyear / (target + 0.0000000000001) * 100)

  # Protect against values greater than 100
  ammended_data <- ammended_data %>% mutate(
    perc = case_when(
      perc > 100 ~ 100,
      TRUE ~ perc
    ),
    perc_week = case_when(
      perc_week > 100 ~ 100,
      TRUE ~ perc_week
    ),
    perc_year = case_when(
      perc_year > 100 ~ 100,
      TRUE ~ perc_year
    )
  )

  # Calculate "Today" within specified Year
  if (cal_type == "cal"){
    start_time <- paste0(for_year, "/01/01")
    PT <- as.numeric((Sys.Date() - as.Date(start_time, "%Y/%m/%d"))) / 365.25 * 100
  } else if (cal_type == "fis"){
    ## Need to make distinction if we're still in the same calendar year or not
    if (month(Sys.Date()) >= 10) {
      PT <- as.numeric(Sys.Date() - as.Date(paste0(for_year - 1, "/10/01"))) / 365.25 * 100

    } else {
      start_time <- paste0(for_year - 1, "/10/01")
      PT <- as.numeric((Sys.Date() - as.Date(start_time, "%Y/%m/%d"))) / 365.25 * 100
    }
  } else{
    start_time <- cal_type
    PT <- as.numeric((Sys.Date() - as.Date(start_time, "%Y/%m/%d"))) / 365.25 * 100
  }

  ## Ensure that it's less than 100 and assign
  if (PT > 100) PT <- 100
  ammended_data <- mutate(ammended_data, percent_time = PT)

  # percent
  ammended_data <- ammended_data %>% mutate(percent_time = case_when(
    percent_time > 100 ~ 100,
    TRUE ~ percent_time
  ))

  # LW and LY text

  # 12/20/18     first do Last Week
  ## and convert any NAs to 0
  ammended_data <- ammended_data %>%
    mutate(actual_lastweek = case_when(
      is.na(actual_lastweek) ~ 0,
      TRUE ~ actual_lastweek
    ))

  ## Calculate behind from LAST
  ammended_data <- ammended_data %>%
    mutate(BehindFromLastWeek = actual - actual_lastweek)

  ## Calculate LAST's percent
  ammended_data <- ammended_data %>%
    mutate(OldPer = actual_lastweek * 100 / target,
           OldPer = case_when(
             OldPer > 100 ~ 100,
             TRUE ~ OldPer
           ))

  ## Calculate how far behind LAST for text
  ammended_data$LWeek_tex[
    ammended_data$BehindFromLastWeek > 0 & !is.na(ammended_data$BehindFromLastWeek)] <- paste("(+",
                                                                                              round(as.numeric(ammended_data$BehindFromLastWeek[ammended_data$BehindFromLastWeek > 0 &
                                                                                                                                                  !is.na(ammended_data$BehindFromLastWeek)])),
                                                                                              " from Last Week)", sep = "")
  ammended_data$LWeek_tex[
    ammended_data$BehindFromLastWeek < 0 & !is.na(ammended_data$BehindFromLastWeek)] <- paste("(",
                                                                                              round(as.numeric(ammended_data$BehindFromLastWeek[ammended_data$BehindFromLastWeek < 0 &
                                                                                                                                                  !is.na(ammended_data$BehindFromLastWeek)])),
                                                                                              " from Last Week)", sep = "")
  # IF BehindFromLastWeek == 0
  ammended_data <- ammended_data %>%
    mutate(LWeek_tex = case_when(
      BehindFromLastWeek == 0 ~ "(No change from Last Week)",
      TRUE ~ LWeek_tex
    ))

  # LY
  ammended_data <- ammended_data %>%
    mutate(actual_lastyear = case_when(
      is.na(actual_lastyear) ~ 0,
      TRUE ~ actual_lastyear
    ))

  ## Calculate behind from LAST

  ammended_data <- ammended_data %>%
    mutate(BehindFromLastYear = actual - actual_lastyear)

  ## Calculate LAST's percent
  ammended_data <- ammended_data %>%
    mutate(LYPer = (actual_lastyear * 100) / target) %>%
    mutate(LYPer = case_when(
      LYPer > 100 ~ 100,
      TRUE ~ LYPer
    ))

  # 12/3/18    works with base R way...
  ammended_data$LY_tex[
    ammended_data$BehindFromLastYear > 0 &
      !is.na(ammended_data$BehindFromLastYear)] <- paste("(+",
                                                         round(as.numeric(ammended_data$BehindFromLastYear[ammended_data$BehindFromLastYear > 0 &
                                                                                                             !is.na(ammended_data$BehindFromLastYear)])),
                                                         " from Last Year)", sep = "")
  ammended_data$LY_tex[
    ammended_data$BehindFromLastYear < 0 &
      !is.na(ammended_data$BehindFromLastYear)] <- paste("(",
                                                         round(as.numeric(ammended_data$BehindFromLastYear[ammended_data$BehindFromLastYear < 0 &
                                                                                                             !is.na(ammended_data$BehindFromLastYear)])),
                                                         " from Last Year)", sep = "")
  # IF BehindFromLastYear == 0
  ammended_data <- ammended_data %>%
    mutate(LY_tex = case_when(
      BehindFromLastYear == 0 ~ "(No change from Last Year)",
      TRUE ~ LY_tex
    ))

  # Value for Indicator lateness or on time
  ammended_data <- ammended_data %>%
    mutate(text = percent_time / 100 * target - actual)

  # Calculate how far behind TODAY the percent for the indicator is
  ammended_data <- ammended_data %>%
    mutate(behind_by = perc - percent_time) %>%
    mutate(text = case_when(
      target == 0 | is.na(target) ~ "No Target!",
      TRUE ~ ""
    ))

  # filter NO Target indicators

  if(remove_no_targets == TRUE) {
    ammended_data <- ammended_data %>%
      filter(!is.na(target), target != 0)
  } else {
    ammended_data
  }

  # Tooltip: hover-over text

  ammended_data <- ammended_data %>%
    mutate(tooltip = glue("{LWeek_tex}; {LY_tex}"),
           tooltip = tooltip %>% str_replace_all(., "\\(|\\)", ""),
           tooltip2 = glue("
                          {LWeek_tex}
                          {LY_tex}")) %>%
    mutate(tooltip2 = tooltip2 %>% str_replace_all("'", "&#39"))


  # Behind By to lower limit = 0
  ammended_data <- ammended_data %>%
    mutate(low_level = -0.2 * percent_time[1])

  low_level <- -0.2 * ammended_data$percent_time[1]
  ammended_data$behind_by[ammended_data$behind_by > 0] <- 0
  ammended_data$behind_by[ammended_data$behind_by < low_level] <- low_level

  return(ammended_data)

}
