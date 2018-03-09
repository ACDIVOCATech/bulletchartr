### These functions allow one to input an excel file and output a nice bulletchart


# internal functions ------------------------------------------------------

#' @title extra_field_calculator
#' @description internal function for calculating the extra fields needed for bullet charts
#' @param file_name path of Excel file
#' @param sheet_name specify which sheet in Excel file
#' @param for_year specify the year in which the report is being made, Default: year(Sys.Date())
#' @param cal_type define what calendar you are using. Options are "fis" for fiscal year starting
#' October 1st, "cal" for calendar year starting January 1st, or enter your own custom date in the
#' format "YYYY/MM/DD", Default: fis
#' @details internal function for calculating the extra fields for the bullet chart
#' @examples
#' @seealso
#'  \code{\link[dplyr]{mutate}}

#'  \code{\link[readxl]{read_excel}}
#' @rdname extra_field_calculator
#' @importFrom dplyr mutate %>% case_when
#' @importFrom testthat test_that expect_equal
#' @importFrom readxl read_xlsx
#' @importFrom lubridate year month

extra_field_calculator <- function(file_name, sheet_name = "Sheet1", for_year = year(Sys.Date()),
                                   cal_type="fis") {

  testthat::test_that("Does the specified file exist in the directory?", {

    testthat::expect_equal(TRUE, file.exists(file_name)
    )

  })

  ## Read in Excel file:
  df <- readxl::read_xlsx(path = file_name, sheet = sheet_name)


  # If df is empty, break function and output empty chart
  if(nrow(df) == 0){
    output_plot <- "No data available"
    return()
  }


  # Create percentage variables
  df <- df %>% dplyr::mutate(perc = actual/(target + 0.0000000000001) * 100,
                      perc_week = actual_lastweek/(target + 0.0000000000001) * 100,
                      perc_year = actual_lastyear/(target + 0.0000000000001) * 100)

  # Protect against values greater than 100
  df <- df %>% mutate(
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
  if(cal_type=="cal"){
    start_time <- paste0(for_year,"/01/01")
  } else if(cal_type=="fis"){
    ## Need to make distinction if we're still in the same calendar year or not
    if(month(Sys.Date()) >= 10) {
      start_time <- paste0(for_year,"/10/01")
      } else {
        start_time <- paste0(for_year-1,"/10/01")
      }
  } else{
    start_time=cal_type
  }

  ## Calculate point in year
  PT <- as.numeric((Sys.Date() -
                      as.Date(start_time, "%Y/%m/%d"))) / 365.25 * 100

  ## Ensure that it's less than 100 and assign
  if(PT>100) PT=100
  df <- mutate(df,percent_time = PT)

  # percent
  df <- df %>% mutate(percent_time = case_when(
    percent_time > 100 ~ 100,
    TRUE ~ percent_time
  ))

  # Value for Indicator lateness or on time
  df <- df %>% mutate(text = percent_time/100 * target - actual)

  # Calculate how far behind TODAY the percent for the indicator is
  df <- df %>% mutate(behind_by = perc - percent_time)

  df <- df %>% mutate(text = case_when(

    behind_by > 0 ~ "OK!",
    behind_by <= 0 & !is.na(behind_by) ~ paste("Need ", round(as.numeric(text)), " more", sep = "")

  ))

  # Behind By to lower limit = 0
  df <- df %>% mutate(low_level = -0.2 * df$percent_time)

  df <- df %>% mutate(behind_by = case_when(
    behind_by > 0 ~ 0,
    behind_by < low_level ~ low_level
  ))

  ## output
  return(df)

}

# plotting functions --------------------------------------------------

# bullet plot Version 1: actual Stephen FEW  -------------------------------------------------

#' @title bullet_chart
#' @description create a Stephen Few bullet chart
#' @param file_name path of Excel file
#' @param sheet_name specify which sheet in Excel file, Default: "Sheet1"
#' @param for_year specify the year in which the report is being made, Default: year(Sys.Date())
#' @param cal_type define what calendar you are using. Options are "fis" for fiscal year starting
#' October 1st, "cal" for calendar year starting January 1st, or enter your own custom date in the
#' format "YYYY/MM/DD", Default: fis
#' @details This version of the bullet chart most closely resembles Stephen Few's design. The single black bar represents
#' the current value of the indicator while the different hue columns represent last week's value (darker hue) and last year's value (lighter hue).
#' @examples
#' bullet_chart3("data/Indicators_targets_ext.xlsx", project_start_date = "2016/03/01")
#' @seealso
#'  \code{\link[ggplot2]{ggplot}}
#' @rdname bullet_chart
#' @export
#' @import ggplot2

bullet_chart <- function(file_name, sheet_name = "Sheet1", for_year = year(Sys.Date()),
                         cal_type="fis") {

  df <- extra_field_calculator(file_name, sheet_name, for_year,
                               cal_type)

  low_level <- df$low_level[1]

  g <- ggplot2::ggplot(df, aes(x = indicator_name)) +
    geom_col(aes(y = 100), fill = "grey85",  width = 0.4) +
    geom_col(aes(y = perc_week), fill = "grey68",  width = 0.4) +
    geom_col(aes(y = perc_year), fill = "#7A7A7A", width = 0.4) +
    #scale_fill_gradient("", limits = c(low_level, 0), low = "#7A7A7A", high = "#B8B8B8") +
    geom_col(aes(y = perc), fill = "grey10", width = 0.1, color = "grey10", alpha = 0.9) +
    geom_text(y = 1, aes(label = text), vjust = -2, hjust = 0, size = 4) +
    geom_hline(yintercept = df$percent_time, alpha = 0.33) +
    annotate("text", x = 0, y = df$percent_time + 1.5, hjust = 0, label = "Today", angle = 90, alpha = 0.5, size = 5) +
    coord_flip() +
    labs(y = "Percent of Yearly Target\n&\n Percent of Year",
         x = " ") +
    ggtitle(paste("Ongoing Indicator Accomplishment (", for_year, ")", sep = "")) +
    theme_minimal() +
    theme(axis.text.y = element_text(size = 15, face = "bold"),
          axis.title.x = element_text(face = "bold", size = 10,
                                      margin = margin(t = 25, r = 0, b = 20, l = 0)),
          axis.text.x = element_text(face = "bold", size = 12),
          title = element_text(face = "bold"),
          plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5, size = 8),
          legend.position = "none") +
    expand_limits(x = 6.75, y = 102)

  print(g)

}

# bullet plot Version 2: multiple width bars -----------------------------------------------

#' @title bullet_chart2
#' @description create bullet chart with bars of varying width
#' @param file_name path of Excel file
#' @param cal_type define what calendar you are using. Options are "fis" for fiscal year starting
#' October 1st, "cal" for calendar year starting January 1st, or enter your own custom date in the
#' format "YYYY/MM/DD", Default: fis
#' @param FY fiscal year or calendar year? Default: TRUE
#' @param project_start_date specify start date of the project as \%Y/\%D/\%M format string
#' @details This version conforms more closely with the standard bullet chart design. This function
#' uses different thicknesses for the bars as the benchmarks for previous time points (last week and last year) to further
#' accentuate the difference graphically.
#' @examples
#' bullet_chart2(file_name = "data/Indicators_targets.xlsx", project_start_date = "2016/03/01")
#'
#' @seealso
#'  \code{\link[ggplot2]{geom_bar}}
#' @rdname bullet_chart2
#' @export
#' @import ggplot2

bullet_chart2 <- function(file_name, sheet_name = "Sheet1", for_year = year(Sys.Date()),
                          cal_type="fis") {

  df <- extra_field_calculator(file_name, sheet_name, for_year,
                               cal_type)

  low_level <- df$low_level[1]

  g <- ggplot2::ggplot(df, aes(x = indicator_name)) +
    geom_col(aes(y = perc_week), width = 0.5, alpha = 0.6) +
    geom_col(aes(y = perc_year), width = 0.75, alpha = 0.3) +
    geom_col(aes(y = perc, fill = behind_by), width = 0.15, color = "black") +
    scale_fill_gradient("Indicator\nBehind By:", limits = c(low_level, 0), low = "red3", high = "green3") +
    geom_text(y = 1, aes(label = text), vjust = -2, hjust = 0, size = 4) +
    geom_hline(yintercept = df$percent_time, alpha = 0.33) +
    annotate("text", x = 0, y = df$percent_time + 1.5, hjust = 0, label = "Today", angle = 90, alpha = 0.5, size = 5) +
    coord_flip() +
    labs(y = "Percent of Yearly Target\n&\n Percent of Year",
         x = " ") +
    ggtitle(paste("Ongoing Indicator Accomplishment (", for_year, ")", sep = "")) +
    theme_minimal() +
    theme(axis.text.y = element_text(size = 15, face = "bold"),
          axis.title.x = element_text(face = "bold", size = 10,
                                      margin = margin(t = 25, r = 0, b = 20, l = 0)),
          axis.text.x = element_text(face = "bold", size = 12),
          title = element_text(face = "bold"),
          plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5, size = 8),
          legend.position = "none") +
    expand_limits(x = 6.75, y = 102)

  print(g)

}



# bullet plot Version 3: symbols ----------------------------------------------------------

#' @title bullet_chart3
#' @description creates bullet chart with symbols
#' @param file_name path of Excel file
#' @param sheet_name specify which sheet in Excel file, Default: "Sheet1"
#' @param cal_type define what calendar you are using. Options are "fis" for fiscal year starting
#' October 1st, "cal" for calendar year starting January 1st, or enter your own custom date in the
#' format "YYYY/MM/DD", Default: fis
#' @param project_start_date specify start date of the project as \%Y/\%D/\%M format string
#' @details The bar for each Indicator show the progression along the horizontal-axis presenting
#' the percentage of the yearly target completed. This axis also shows the percent of the year
#' gone by with the vertical line indicating what exact percentage "Today" is, along this percentage.
#'
#' Each bar is colored "green" if the Indicator completion is close to or past "Today", "orange" if
#' it's close, and "red" if it is far off track.
#'
#' The symbols represent the indicator value for last week (diamond) and last year (circle).
#'
#' @examples
#'  bullet_chart(file_name = "data/Indicators_targets.xlsx", project_start_date = "2016/03/01")
#' @seealso
#'  \code{\link[ggplot2]{geom_bar}}, \code{\link[ggplot2]{scale_manual}}
#' @rdname bullet_chart3
#' @export
#' @import ggplot2

bullet_chart3 <- function(file_name, sheet_name = "Sheet1", for_year = year(Sys.Date()),
                          cal_type="fis") {

  df <- extra_field_calculator(file_name, sheet_name, for_year,
                               cal_type)

  low_level <- df$low_level[1]

  g <- ggplot2::ggplot(df, aes(x = indicator_name)) +
    geom_col(aes(y = perc, fill = behind_by), width = 0.1, color = "black") +
    scale_fill_gradient("Indicator\nBehind By:", limits = c(low_level, 0), low = "red", high = "green",
                        guide = FALSE) +
    geom_point(aes(y = perc_week, shape = "Last Week"), size = 6, stroke = 1) +
    geom_point(aes(y = perc_year, shape = "Last Year"), size = 6, stroke = 1) +
    scale_shape_manual(" ", values = c(23, 21)) +
    geom_col(aes(y = 100), width = 0.5, alpha = 0.25) +
    geom_text(y = 1, aes(label = text), vjust = -1.5, hjust = 0) +
    geom_hline(yintercept = df$percent_time, alpha = 0.33) +
    annotate("text", x = 0, y = df$percent_time + 1.5, hjust = 0, label = "Today",
             angle = 90, alpha = 0.5, size = 5) +
    coord_flip() +
    labs(y = "Percent of Yearly Target\n&\n Percent of Year",
         x = " ") +
    ggtitle(paste("Ongoing Indicator Accomplishment (", for_year, ")", sep = "")) +
    theme_minimal() +
    theme(axis.text.y = element_text(size = 15, face = "bold"),
          axis.title.x = element_text(face = "bold", size = 10,
                                      margin = margin(t = 25, r = 0, b = 20, l = 0)),
          axis.text.x = element_text(face = "bold", size = 14),
          title = element_text(face = "bold", size = 14),
          plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5, size = 8),
          legend.position = c(0.8, -0.12),
          legend.key.size = unit(1.5, "lines")) +
    expand_limits(x = 6.75, y = 102)

  print(g)

}


# bullet chart Version 4: last year LINE ----------------------------------

#' @title bullet_chart4
#' @description create bullet chart showing last year's value as the target
#' @param file_name path of Excel file
#' @param sheet_name specify which sheet in Excel file, Default: "Sheet1"
#' @param for_year specify the year in which the report is being made, Default: year(Sys.Date())
#' @param cal_type define what calendar you are using. Options are "fis" for fiscal year starting
#' October 1st, "cal" for calendar year starting January 1st, or enter your own custom date in the
#' format "YYYY/MM/DD", Default: fis
#' @details This version of the bullet chart shows a single colored bar representing the current value
#' for the indicator along with a black vertical line representing the indicator value at this time
#' last year. The definition for the vertical line can be changed to your preference (such as a more
#' general "target" value), however at the current time you should change the values of "actual_lastyear"
#' in the Excel file but not the variable name itself.
#' @examples
#' bullet_chart4("data/Indicators_targets_ext.xlsx", project_start_date = "2016/03/01")
#' @seealso
#'  \code{\link[ggplot2]{ggplot}}
#' @rdname bullet_chart4
#' @export
#' @import ggplot2

bullet_chart4 <- function(file_name, sheet_name = "Sheet1", for_year = year(Sys.Date()),
                          cal_type="fis") {

  df <- extra_field_calculator(file_name, sheet_name, for_year,
                               cal_type)

  low_level <- df$low_level[1]

  g <- ggplot2::ggplot(df, aes(x = indicator_name)) +
    geom_col(aes(y = perc, fill = behind_by), width = 0.15, color = "black") +
    scale_fill_gradient("", limits = c(low_level, 0), low = "darkred", high = "darkgreen") +
    geom_point(aes(y = perc_year, shape = "Last Year"), size = 4.5, stroke = 3) +
    scale_shape_manual(" ", values = 124) +
    geom_col(aes(y = 100), width = 0.5, alpha = 0.25) +
    geom_text(y = 1, aes(label = text), vjust = -1.5, hjust = 0) +
    coord_flip() +
    labs(y = "Percent of Yearly Target",
         x = " ") +
    ggtitle(paste("Ongoing Indicator Accomplishment (", for_year, ")", sep = "")) +
    theme_minimal() +
    theme(axis.text.y = element_text(size = 15, face = "bold"),
          axis.title.x = element_text(face = "bold", size = 10,
                                      margin = margin(t = 25, r = 0, b = 20, l = 0)),
          axis.text.x = element_text(face = "bold", size = 12),
          title = element_text(face = "bold"),
          plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5, size = 8),
          legend.position = "none") +
    expand_limits(x = 6.75, y = 102)

  print(g)

}





