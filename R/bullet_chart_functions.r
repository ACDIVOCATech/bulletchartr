### These functions allow one to input an excel file and output a nice bulletchart


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
#' October 1st, "cal" for calendar year starting January 1st, or enter your own custom date in the
#' format "YYYY/MM/DD", Default: fis
#' @details internal function for calculating the extra fields for the bullet chart
#' @seealso
#'  \code{\link[dplyr]{mutate}}
#'  \code{\link[readxl]{read_excel}}
#' @rdname extra_field_calculator
#' @importFrom dplyr mutate %>% case_when
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
                                   cal_type = "fis") {


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
      # start_time <- paste0(for_year, "/10/01")
      PT <- as.numeric(Sys.Date() - as.Date(paste0(for_year - 1, "/10/01"))) / 365.25 * 100

      # Sys.Date() - as.Date(paste0(for_year - 1, "/10/01"))
      # (Sys.Date() - as.Date(paste(format(as.Date(forThisDB$FROM), "%d %b"), forYear - 1), format = "%d %b %Y"))
      } else {
        start_time <- paste0(for_year - 1, "/10/01")
        PT <- as.numeric((Sys.Date() - as.Date(start_time, "%Y/%m/%d"))) / 365.25 * 100
      }
  } else{
    start_time <- cal_type
    PT <- as.numeric((Sys.Date() - as.Date(start_time, "%Y/%m/%d"))) / 365.25 * 100
  }

  ## Calculate point in year
  #PT <- as.numeric((Sys.Date() - as.Date(start_time, "%Y/%m/%d"))) / 365.25 * 100

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

  # >>>>>>>>>>>>>>>>>>

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

  # 12.21.2018 delete text
  # ammended_data <- ammended_data %>%
  #   mutate(text = case_when(
  #
  #     behind_by > 0 ~ "OK!",
  #     behind_by <= 0 & !is.na(behind_by) ~ paste("Need ", round(as.numeric(text)), " more", sep = "")
  #
  #   )) %>%
    # mutate(text = case_when(
    #   target == 0 | is.na(target) ~ "No Target!",
    #   TRUE ~ text
    # ))

  # Tooltip: hover-over text

  ammended_data <- ammended_data %>%
    mutate(tooltip = glue("
                          {LWeek_tex}
                          {LY_tex}")) %>%
    mutate(tooltip = tooltip %>% str_replace_all("'", "&#39"))


  # Behind By to lower limit = 0
  ammended_data <- ammended_data %>%
    mutate(low_level = -0.2 * percent_time[1])

  low_level <- -0.2 * ammended_data$percent_time[1]
  ammended_data$behind_by[ammended_data$behind_by > 0] <- 0
  ammended_data$behind_by[ammended_data$behind_by < low_level] <- low_level
  # ammended_data <- ammended_data %>%
  #   mutate(behind_by = case_when(
  #     behind_by > 0 ~ 0,
  #     behind_by < low_level ~ low_level
  #   ))

  ## output
  return(ammended_data)

}

# plotting functions --------------------------------------------------

# bullet plot Version 1: actual Stephen FEW  -------------------------------------------------

#' @title bullet_chart
#' @description create a Stephen Few bullet chart
#' @param file_name path of Excel file
#' @param sheet_name specify which sheet in Excel file, Default: "Sheet1"
#' @param dataframe specify R dataframe input
#' @param indicator_name specify the name of the column that has your indicator/KPI names
#' @param actual specify the name of the column that has the current value of your indicators/KPIs
#' @param actual_lastweek specify the name of the column that has the indicator/KPI value from the previous week
#' @param actual_lastyear specify the name of the column that has the indicator/KPI value from the previous year
#' @param target specify the name of the column that has the target value for the indicator/KPI
#' @param for_year specify the year in which the report is being made, Default: year(Sys.Date())
#' @param cal_type define what calendar you are using. Options are "fis" for fiscal year starting
#' October 1st, "cal" for calendar year starting January 1st, or enter your own custom date in the
#' format "YYYY/MM/DD", Default: fis
#' @param small specify whether you want the small version of the plot (TRUE or FALSE), Default: FALSE
#' @param legend specify whether you want to show the legend, Default: FALSE
#' @details This version of the bullet chart most closely resembles Stephen Few's design. The single black bar represents
#' the current value of the indicator while the different hue columns represent last week's value (darker hue) and last year's value (lighter hue).
#' @examples
#' data(df)
#' bullet_chart(dataframe = df)
#' @seealso
#'  \code{\link[ggplot2]{ggplot}}
#' @rdname bullet_chart
#' @export
#' @importFrom ggplot2 ggplot aes geom_col geom_hline coord_flip labs ggtitle theme_minimal
#' expand_limits scale_alpha_manual geom_text annotate theme element_text margin unit
#' @importFrom dplyr mutate %>% select
#' @importFrom ggiraph geom_bar_interactive ggiraph girafe

bullet_chart <- function(file_name = NULL, sheet_name = "Sheet1",
                         dataframe = NULL,
                         indicator_name = "indicator_name",
                         actual = "actual",
                         actual_lastweek = "actual_lastweek",
                         actual_lastyear = "actual_lastyear",
                         target = "target",
                         for_year = year(Sys.Date()),
                         cal_type = "fis",
                         small = FALSE, legend = FALSE) {

  ammended_data <- extra_field_calculator(file_name, sheet_name,
                                          dataframe,
                                          indicator_name, actual,
                                          actual_lastweek, actual_lastyear,
                                          target, for_year, cal_type)

  g <- ggplot(ammended_data, aes(x = indicator_name)) +
    geom_col(aes(y = 100), fill = "grey85", width = 0.4) +
    geom_hline(yintercept = ammended_data$percent_time, alpha = 0.33) +
    coord_flip() +
    labs(y = "Percent of Yearly Target\n&\n Percent of Year",
         x = " ") +
    ggtitle(paste("Ongoing Indicator Accomplishment (", for_year, ")", sep = "")) +
    theme_minimal() +
    expand_limits(x = nrow(ammended_data) + 1.25, y = 102)

  #if (small != is.logical(small)) warning("small must be TRUE or FALSE")

  if (small == FALSE){

    g <- g + geom_col(aes(y = perc_week, alpha = "lastweek"), width = 0.4) +
      geom_col(aes(y = perc_year, alpha = "lastyear"), width = 0.4) +
      scale_alpha_manual(name = "",
                         values = c(0.6, 0.3),
                         labels = c("lastweek" = "Last Week", "lastyear" = "Last Year")) +
      #geom_col(aes(y = perc), fill = "grey10", width = 0.1, color = "grey10", alpha = 0.9) +
      geom_bar_interactive(aes(x = indicator_name, y = perc,
                               tooltip = tooltip,
                               data_id = indicator_name),
                           stat = "identity", alpha = 0.9,
                           fill = "grey10",
                           width = 0.1, color = "grey10") +

      geom_text(y = 1, aes(label = text), vjust = -2, hjust = 0, size = 4) +
      annotate("text", x = 0, y = ammended_data$percent_time + 1.5,
               hjust = 0, label = "Today", angle = 90, alpha = 0.5, size = 5) +
      theme(axis.text.y = element_text(size = 15, face = "bold"),
            axis.title.x = element_text(face = "bold", size = 10,
                                        margin = margin(t = 25, r = 0, b = 20, l = 0)),
            axis.text.x = element_text(face = "bold", size = 12),
            title = element_text(face = "bold"),
            plot.title = element_text(hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5, size = 8))

    if (legend == FALSE){

      g <- g + theme(legend.position = "none")

      output <- girafe(code = {print(g)},
                       width = 0.5
      )
      output

    }else if (legend == TRUE){

      g <- g + guides(shape = guide_legend(nrow = 1)) + theme(legend.position = "bottom")
      output <- girafe(code = {print(g)},
                       width = 0.5
      )
      output

    }

  }else if (small == TRUE){

    g <- g + geom_col(aes(y = perc_week, alpha = "lastweek"), width = 0.4) +
      geom_col(aes(y = perc_year, alpha = "lastyear"), width = 0.4) +
      scale_alpha_manual(name = "",
                         values = c(0.6, 0.3),
                         labels = c("lastweek" = "Last Week", "lastyear" = "Last Year")) +
      #geom_col(aes(y = perc), fill = "grey10", width = 0.15, color = "grey10", alpha = 0.9) +
      geom_bar_interactive(aes(x = indicator_name, y = perc,
                               tooltip = tooltip,
                               data_id = indicator_name),
                           stat = "identity", alpha = 0.9,
                           fill = "grey10",
                           width = 0.1, color = "grey10") +
      annotate("text", x = 0, y = ammended_data$percent_time + 1.5, hjust = 0, label = "Today",
               angle = 90, alpha = 0.5, size = 2.5) +
      theme(axis.text.y = element_text(size = 8, face = "bold"),
            axis.title.x = element_text(face = "bold", size = 7,
                                        margin = margin(t = 25, r = 0, b = 20, l = 0)),
            axis.text.x = element_text(face = "bold", size = 10),
            title = element_text(face = "bold", size = 8),
            plot.title = element_text(hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5, size = 6),
            legend.text = element_text(size = 8),
            legend.key.size = unit(0.8, "lines"))

    if (legend == FALSE){

      g <- g + theme(legend.position = "none")

      output <- girafe(code = {print(g)},
                       width = 0.5
      )
      output

    }else if (legend == TRUE){

      g <- g + guides(shape = guide_legend(nrow = 1)) + theme(legend.position = "bottom")
      output <- girafe(code = {print(g)},
                       width = 0.5
      )
      output

    }
  }
}

# bullet plot Version 2: multiple width bars -----------------------------------------------

#' @title bullet_chart_wide
#' @description create bullet chart with bars of varying width
#' @param file_name path of Excel file
#' @param sheet_name specify which sheet in Excel file, Default: "Sheet1"
#' @param dataframe specify R dataframe input
#' @param indicator_name specify the name of the column that has your indicator/KPI names
#' @param actual specify the name of the column that has the current value of your indicators/KPIs
#' @param actual_lastweek specify the name of the column that has the indicator/KPI value from the previous week
#' @param actual_lastyear specify the name of the column that has the indicator/KPI value from the previous year
#' @param target specify the name of the column that has the target value for the indicator/KPI
#' @param for_year specify the year in which the report is being made, Default: year(Sys.Date())
#' @param cal_type define what calendar you are using. Options are "fis" for fiscal year starting
#' October 1st, "cal" for calendar year starting January 1st, or enter your own custom date in the
#' format "YYYY/MM/DD", Default: fis
#' @param small specify whether you want the small version of the plot (TRUE or FALSE), Default: FALSE
#' @param legend specify whether you want to show the legend, Default: FALSE
#' @details This version conforms more closely with the standard bullet chart design. This function
#' uses different thicknesses for the bars as the benchmarks for previous time points (last week and last year) to further
#' accentuate the difference graphically.
#' @examples
#' data(df)
#' bullet_chart_wide(dataframe = df)
#' @seealso
#'  \code{\link[ggplot2]{geom_bar}}
#' @rdname bullet_chart_wide
#' @export
#' @importFrom ggplot2 ggplot aes geom_col geom_hline coord_flip labs ggtitle theme_minimal
#' expand_limits scale_alpha_manual scale_fill_gradient geom_text annotate theme
#' element_text margin unit
#' @importFrom dplyr mutate %>% select
#' @importFrom ggiraph geom_bar_interactive ggiraph girafe

bullet_chart_wide <- function(file_name = NULL, sheet_name = "Sheet1",
                              dataframe = NULL,
                              indicator_name = "indicator_name",
                              actual = "actual",
                              actual_lastweek = "actual_lastweek",
                              actual_lastyear = "actual_lastyear",
                              target = "target",
                              for_year = year(Sys.Date()),
                              cal_type = "fis",
                              small = FALSE, legend = FALSE) {

  ammended_data <- extra_field_calculator(file_name, sheet_name,
                                          dataframe,
                                          indicator_name, actual,
                                          actual_lastweek, actual_lastyear,
                                          target, for_year, cal_type)
  low_level <- ammended_data$low_level[1]


  g <- ggplot(ammended_data, aes(x = indicator_name)) +
    geom_col(aes(y = perc_week, alpha = "lastweek"), width = 0.5) +
    geom_hline(yintercept = ammended_data$percent_time, alpha = 0.33) +
    coord_flip() +
    labs(y = "Percent of Yearly Target\n&\n Percent of Year",
         x = " ") +
    ggtitle(paste("Ongoing Indicator Accomplishment (", for_year, ")", sep = "")) +
    theme_minimal() +
    expand_limits(x = nrow(ammended_data) + 1.25, y = 102)

  if (small == FALSE){

    g <- g + geom_col(aes(y = perc_year, alpha = "lastyear"), width = 0.75) +
      scale_alpha_manual(name = "",
                         values = c(0.6, 0.3),
                         labels = c("lastweek" = "Last Week", "lastyear" = "Last Year")) +
      #geom_col(aes(y = perc, fill = behind_by), width = 0.15, color = "black") +
      geom_bar_interactive(aes(x = indicator_name, y = perc,
                               tooltip = tooltip, alpha = "lastweek",
                               data_id = indicator_name,
                               fill = behind_by),
                           stat = "identity",
                           width = 0.15, color = "black") +
      scale_fill_gradient("", limits = c(low_level, 0),
                          low = "red3", high = "green3",
                          guide = FALSE,
                          labels = c("Very Behind Schedule", "Behind Schedule", "Slightly Behind", "On Time"),
                          breaks = c(low_level + 1.5, low_level + 4.15, low_level + 6.25, low_level + 8.5)) +
      # guides(fill = guide_colorbar(frame.colour = "black", frame.linetype = "solid",
      #                              direction = "vertical")) +
      geom_text(y = 1, aes(label = text), vjust = -2, hjust = 0, size = 4) +
      annotate("text", x = 0, y = ammended_data$percent_time + 1.5,
               hjust = 0, label = "Today", angle = 90, alpha = 0.5, size = 5) +
      theme(axis.text.y = element_text(size = 15, face = "bold"),
            axis.title.x = element_text(face = "bold", size = 10,
                                        margin = margin(t = 25, r = 0, b = 20, l = 0)),
            axis.text.x = element_text(face = "bold", size = 12),
            title = element_text(face = "bold"),
            plot.title = element_text(hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5, size = 8))

    if (legend == FALSE){

      g <- g + theme(legend.position = "none")

      output <- girafe(code = {print(g)},
                       width = 0.5
      )
      output

    }else if (legend == TRUE){

      g <- g + guides(shape = guide_legend(nrow = 1)) + theme(legend.position = "bottom")
      output <- girafe(code = {print(g)},
                       width = 0.5
      )
      output

    }

  }else if (small == TRUE){

    g <- g + geom_col(aes(y = perc_year, alpha = "lastyear"), width = 0.65) +
      scale_alpha_manual(name = "",
                         values = c(0.6, 0.3),
                         labels = c("lastweek" = "Last Week", "lastyear" = "Last Year")) +
      #geom_col(aes(y = perc, fill = behind_by), width = 0.15, color = "black") +
      geom_bar_interactive(aes(x = indicator_name, y = perc,
                               tooltip = tooltip, alpha = "lastweek",
                               data_id = indicator_name,
                               fill = behind_by),
                           stat = "identity",
                           width = 0.15, color = "black") +
      scale_fill_gradient(" ", limits = c(low_level, 0),
                          low = "red3", high = "green3",
                          guide = FALSE,
                          labels = c("Very Behind Schedule", "Behind Schedule", "Slightly Behind", "On Time"),
                          breaks = c(low_level + 1.5, low_level + 4.15, low_level + 6.25, low_level + 8.5)) +
      # guides(fill = guide_colorbar(frame.colour = "black", frame.linetype = "solid",
      #                              direction = "vertical")) +
      annotate("text", x = 0, y = ammended_data$percent_time + 1.5, hjust = 0, label = "Today",
               angle = 90, alpha = 0.5, size = 2.5) +
      theme(axis.text.y = element_text(size = 8, face = "bold"),
            axis.title.x = element_text(face = "bold", size = 7,
                                        margin = margin(t = 25, r = 0, b = 20, l = 0)),
            axis.text.x = element_text(face = "bold", size = 10),
            title = element_text(face = "bold", size = 8),
            plot.title = element_text(hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5, size = 6),
            legend.text = element_text(size = 8),
            legend.key.size = unit(0.8, "lines"))

    if (legend == FALSE){

      g <- g + theme(legend.position = "none")

      output <- girafe(code = {print(g)},
                       width = 0.5
      )
      output

    }else if (legend == TRUE){

      g <- g + guides(shape = guide_legend(nrow = 1)) + theme(legend.position = "bottom")
      output <- girafe(code = {print(g)},
                       width = 0.5
      )
      output

    }
  }
}



# bullet plot Version 3: symbols ----------------------------------------------------------

#' @title bullet_chart_symbols
#' @description creates bullet chart with symbols
#' @param file_name path of Excel file
#' @param sheet_name specify which sheet in Excel file, Default: "Sheet1"
#' @param dataframe specify R dataframe input
#' @param indicator_name specify the name of the column that has your indicator/KPI names
#' @param actual specify the name of the column that has the current value of your indicators/KPIs
#' @param actual_lastweek specify the name of the column that has the indicator/KPI value from the previous week
#' @param actual_lastyear specify the name of the column that has the indicator/KPI value from the previous year
#' @param target specify the name of the column that has the target value for the indicator/KPI
#' @param for_year specify the year in which the report is being made, Default: year(Sys.Date())
#' @param cal_type define what calendar you are using. Options are "fis" for fiscal year starting
#' October 1st, "cal" for calendar year starting January 1st, or enter your own custom date in the
#' format "YYYY/MM/DD", Default: fis
#' @param small specify whether you want the small version of the plot (TRUE or FALSE), Default: FALSE
#' @param legend specify whether you want to show the legend, Default: FALSE
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
#' data(df)
#' bullet_chart_symbols(dataframe = df)
#' @seealso
#'  \code{\link[ggplot2]{geom_bar}}, \code{\link[ggplot2]{scale_manual}}
#' @rdname bullet_chart_symbols
#' @export
#' @importFrom ggplot2 ggplot aes geom_col geom_hline coord_flip labs ggtitle theme_minimal
#' expand_limits scale_fill_gradient scale_shape_manual geom_text annotate theme
#' element_text margin unit geom_point guides guide_legend
#' @importFrom dplyr mutate %>% select
#' @importFrom ggiraph geom_bar_interactive ggiraph girafe

bullet_chart_symbols <- function(file_name = NULL, sheet_name = "Sheet1",
                                 dataframe = NULL,
                                 indicator_name = "indicator_name",
                                 actual = "actual",
                                 actual_lastweek = "actual_lastweek",
                                 actual_lastyear = "actual_lastyear",
                                 target = "target",
                                 for_year = year(Sys.Date()),
                                 cal_type = "fis",
                                 small = FALSE, legend = FALSE) {

  ammended_data <- extra_field_calculator(file_name, sheet_name,
                                          dataframe,
                                          indicator_name, actual,
                                          actual_lastweek, actual_lastyear,
                                          target, for_year, cal_type)

  low_level <- ammended_data$low_level[1]

  g <- ggplot(ammended_data) +
    # 100% bar   NOTE: order is important, have interactive after or won't be able to hover-over
    geom_col(aes(x = indicator_name, y = 100),
             width = 0.5, alpha = 0.25) +
    # interactive
    geom_bar_interactive(aes(x = indicator_name, y = perc,
                             tooltip = tooltip,
                             data_id = indicator_name,
                             fill = behind_by),
                         stat = "identity",
                         width = 0.15, color = "black") +
    # Today
    geom_hline(yintercept = ammended_data$percent_time, alpha = 0.33, size = 1.25) +
    coord_flip() +
    labs(y = "Percent of Yearly Target\n&\n Percent of Year",
         x = " ") +
    ggtitle(paste("Ongoing Indicator Accomplishment (", for_year, ")", sep = "")) +
    theme_minimal() +
    expand_limits(x = nrow(ammended_data) + 1.25, y = 102)

  if (small == FALSE){

    g <- g + scale_fill_gradient("", limits = c(low_level, 0),
                                 low = "red", high = "green",
                                 guide = FALSE,
                                 labels = c("Very Behind Schedule", "Behind Schedule", "Slightly Behind", "On Time"),
                                 breaks = c(low_level + 1.5, low_level + 4.15, low_level + 6.25, low_level + 8.5)) +
      # guides(fill = guide_colorbar(frame.colour = "black", frame.linetype = "solid",
      #                              direction = "vertical")) +
      geom_point(aes(x = indicator_name, y = perc_week, shape = "Last Week"),
                 size = 5, stroke = 1) +
      geom_point(aes(x = indicator_name, y = perc_year, shape = "Last Year"),
                 size = 5, stroke = 1) +
      scale_shape_manual(" ", values = c(23, 21)) +
      geom_text(y = 1, aes(x = indicator_name, label = text), vjust = -1, hjust = 0, size = 4) +
      annotate("text", x = 0, y = ammended_data$percent_time + 1.5, hjust = 0, label = "Today",
               angle = 90, alpha = 0.5, size = 5) +
      theme(axis.text.y = element_text(size = 15, face = "bold"),
            axis.title.x = element_text(face = "bold", size = 10,
                                        margin = margin(t = 25, r = 0, b = 20, l = 0)),
            axis.text.x = element_text(face = "bold", size = 14),
            title = element_text(face = "bold", size = 14),
            plot.title = element_text(hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5, size = 8),
            legend.key.size = unit(1.5, "lines"))

    if (legend == FALSE){

      g <- g + theme(legend.position = "none")

      output <- girafe(code = {print(g)},
                       width = 0.5
      )
      output

    }else if (legend == TRUE){

      g <- g + guides(shape = guide_legend(nrow = 1)) + theme(legend.position = "bottom")
      output <- girafe(code = {print(g)},
                       width = 0.5
      )
      output

    }

  }else if (small == TRUE){

    g <- g +
      scale_fill_gradient(" ", limits = c(low_level, 0),
                          low = "red", high = "green",
                          guide = FALSE,
                          labels = c("Very Behind Schedule", "Behind Schedule", "Slightly Behind", "On Time"),
                          breaks = c(low_level + 1.5, low_level + 4.15, low_level + 6.25, low_level + 8.5)) +
      # guides(fill = guide_colorbar(frame.colour = "black", frame.linetype = "solid",
      #                              direction = "vertical")) +
      geom_point(aes(x = indicator_name, y = perc_week, shape = "Last Week"),
                 size = 3, stroke = 1) +
      geom_point(aes(x = indicator_name, y = perc_year, shape = "Last Year"),
                 size = 3, stroke = 1) +
      scale_shape_manual(" ", values = c(23, 21)) +
      geom_text(y = 1, aes(x = indicator_name, label = text), vjust = -1, hjust = 0, size = 2) +
      annotate("text", x = 0, y = ammended_data$percent_time + 1.5, hjust = 0, label = "Today",
               angle = 90, alpha = 0.5, size = 2.5) +
      theme(axis.text.y = element_text(size = 8, face = "bold"),
            axis.title.x = element_text(face = "bold", size = 7,
                                        margin = margin(t = 25, r = 0, b = 20, l = 0)),
            axis.text.x = element_text(face = "bold", size = 10),
            title = element_text(face = "bold", size = 8),
            plot.title = element_text(hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5, size = 6),
            legend.text = element_text(size = 8),
            legend.key.size = unit(0.8, "lines"))

    if (legend == FALSE){

      g <- g + theme(legend.position = "none")

      output <- girafe(code = {print(g)},
                       width = 0.4
      )
      output

    }else if (legend == TRUE){

      g <- g + guides(shape = guide_legend(nrow = 1)) + theme(legend.position = "bottom")
      output <- girafe(code = {print(g)},
                       width = 0.4
      )
      output

    }
  }
}


# bullet chart Version 4: last year LINE ----------------------------------

#' @title bullet_chart_vline
#' @description create bullet chart showing last year's value as the target
#' @param file_name path of Excel file
#' @param sheet_name specify which sheet in Excel file, Default: "Sheet1"
#' @param dataframe specify R dataframe input
#' @param indicator_name specify the name of the column that has your indicator/KPI names
#' @param actual specify the name of the column that has the current value of your indicators/KPIs
#' @param actual_lastweek specify the name of the column that has the indicator/KPI value from the previous week
#' @param actual_lastyear specify the name of the column that has the indicator/KPI value from the previous year
#' @param target specify the name of the column that has the target value for the indicator/KPI
#' @param for_year specify the year in which the report is being made, Default: year(Sys.Date())
#' @param cal_type define what calendar you are using. Options are "fis" for fiscal year starting
#' October 1st, "cal" for calendar year starting January 1st, or enter your own custom date in the
#' format "YYYY/MM/DD", Default: fis
#' @param small specify whether you want the small version of the plot (TRUE or FALSE), Default: FALSE
#' @param legend specify whether you want to show the legend, Default: FALSE
#' @details This version of the bullet chart shows a single colored bar representing the current value
#' for the indicator along with a black vertical line representing the indicator value at this time
#' last year. The definition for the vertical line can be changed to your preference (such as a more
#' general "target" value), however at the current time you should change the values of "actual_lastyear"
#' in the Excel file but not the variable name itself.
#' @examples
#' data(df)
#' bullet_chart_vline(dataframe = df)
#' @seealso
#'  \code{\link[ggplot2]{ggplot}}
#' @rdname bullet_chart_vline
#' @export
#' @importFrom ggplot2 ggplot aes geom_col geom_hline coord_flip labs ggtitle theme_minimal
#' expand_limits scale_alpha_manual scale_fill_gradient geom_text annotate theme
#' element_text margin unit geom_point
#' @importFrom dplyr mutate %>% select
#' @importFrom ggiraph geom_bar_interactive ggiraph girafe

bullet_chart_vline <- function(file_name = NULL, sheet_name = "Sheet1",
                               dataframe = NULL,
                               indicator_name = "indicator_name",
                               actual = "actual",
                               actual_lastweek = "actual_lastweek",
                               actual_lastyear = "actual_lastyear",
                               target = "target",
                               for_year = year(Sys.Date()),
                               cal_type = "fis",
                               small = FALSE, legend = FALSE) {

  ammended_data <- extra_field_calculator(file_name, sheet_name,
                                          dataframe,
                                          indicator_name, actual,
                                          actual_lastweek, actual_lastyear,
                                          target, for_year, cal_type)

  low_level <- ammended_data$low_level[1]

  g <- ggplot(ammended_data, aes(x = indicator_name)) +
    geom_col(aes(y = 100), width = 0.5, alpha = 0.25) +
    #geom_col(aes(y = perc, fill = behind_by), width = 0.15, color = "black") +
    geom_bar_interactive(aes(x = indicator_name, y = perc,
                             tooltip = tooltip,
                             data_id = indicator_name,
                             fill = behind_by),
                         stat = "identity",
                         width = 0.15, color = "black") +
    scale_fill_gradient("", limits = c(low_level, 0),
                        low = "red", high = "green",
                        guide = FALSE,
                        labels = c("Very Behind Schedule", "Behind Schedule", "Slightly Behind", "On Time"),
                        breaks = c(low_level + 1.5, low_level + 4.15, low_level + 6.25, low_level + 8.5)) +
    # guides(fill = guide_colorbar(frame.colour = "black", frame.linetype = "solid",
    #                              direction = "vertical")) +
    coord_flip() +
    labs(y = "Percent of Yearly Target",
         x = " ") +
    ggtitle(paste("Ongoing Indicator Accomplishment (", for_year, ")", sep = "")) +
    theme_minimal() +
    expand_limits(x = nrow(ammended_data) + 1.25, y = 102)

  if (small == FALSE){

    g <- g + geom_point(aes(y = perc_year, shape = "Last Year"), size = 4.5, stroke = 3) +
      scale_shape_manual(" ", values = 124) +
      geom_text(y = 1, aes(label = text), vjust = -1.5, hjust = 0) +
      theme(axis.text.y = element_text(size = 15, face = "bold"),
            axis.title.x = element_text(face = "bold", size = 10,
                                        margin = margin(t = 25, r = 0, b = 20, l = 0)),
            axis.text.x = element_text(face = "bold", size = 12),
            title = element_text(face = "bold"),
            plot.title = element_text(hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5, size = 8))

    if (legend == FALSE){

      g <- g + theme(legend.position = "none")

      output <- girafe(code = {print(g)},
                       width = 0.4
      )
      output

    }else if (legend == TRUE){

      g <- g + guides(shape = guide_legend(nrow = 1)) + theme(legend.position = "bottom")
      output <- girafe(code = {print(g)},
                       width = 0.4
      )
      output

    }

  } else if (small == TRUE){

    g <- g + geom_point(aes(y = perc_year, shape = "Last Year"), size = 3, stroke = 3) +
      scale_shape_manual(" ", values = 124) +
      theme(axis.text.y = element_text(size = 8, face = "bold"),
            axis.title.x = element_text(face = "bold", size = 7,
                                        margin = margin(t = 25, r = 0, b = 20, l = 0)),
            axis.text.x = element_text(face = "bold", size = 10),
            title = element_text(face = "bold", size = 8),
            plot.title = element_text(hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5, size = 6),
            legend.text = element_text(size = 8),
            legend.key.size = unit(0.8, "lines"))

    if (legend == FALSE){

      g <- g + theme(legend.position = "none")

      output <- girafe(code = {print(g)},
                       width = 0.4
      )
      output

    }else if (legend == TRUE){

      g <- g + guides(shape = guide_legend(nrow = 1)) + theme(legend.position = "bottom")
      output <- girafe(code = {print(g)},
                       width = 0.4
      )
      output

    }
  }
}
