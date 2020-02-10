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
#' @param chart_type static of interactive (ggiraph) version
#' @param small specify whether you want the small version of the plot (TRUE or FALSE), Default: FALSE
#' @param legend specify whether you want to show the legend, Default: TRUE
#' @param remove_no_targets remove indicators with Targets == NA or 0, Default: FALSE
#' @param show_text Show 'Last Week' & 'Last Year' text, when `small = TRUE` or
#' `chart_type = "interactive"` then no text will be shown by default.
#' @details This version of the bullet chart shows a single colored bar representing the current value
#' for the indicator along with a black vertical line representing the indicator value at this time
#' last year. The definition for the vertical line can be changed to your preference (such as a more
#' general "target" value), however at the current time you should change the values of "actual_lastyear"
#' in the Excel file but not the variable name itself.
#' @examples
#' data("bc_ex_TC")
#' bullet_chart_symbols(dataframe = bc_ex_TC)
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
                               chart_type = "static",
                               small = FALSE, legend = TRUE,
                               remove_no_targets = FALSE,
                               show_text = FALSE) {

  ammended_data <- extra_field_calculator(file_name, sheet_name,
                                          dataframe,
                                          indicator_name, actual,
                                          actual_lastweek, actual_lastyear,
                                          target, for_year, cal_type,
                                          remove_no_targets)
  ## for vline we don't want LW data??

  low_level <- ammended_data$low_level[1]

  ## check for Target == 0 in all Targets
  if (all(ammended_data$target == 0)) {
    return(
      "No Non-Zero Targets!"
    )
  }

  if (chart_type == "static") {

    g <- ggplot(ammended_data, aes(x = indicator_name)) +
      geom_col(aes(y = perc, fill = behind_by), width = 0.15, color = "black") +
      scale_fill_gradient("", limits = c(low_level, 0),
                          low = "red", high = "green",
                          guide = FALSE,
                          labels = c("Very Behind Schedule", "Behind Schedule", "Slightly Behind", "On Time"),
                          breaks = c(low_level + 1.5, low_level + 4.15, low_level + 6.25, low_level + 8.5)) +
      geom_col(aes(y = 100), width = 0.5, alpha = 0.25) +
      geom_hline(yintercept = ammended_data$percent_time, alpha = 0.33) +
      coord_flip() +
      labs(y = "Percent of Yearly Target\n&\n Percent of Year",
           x = " ") +
      ggtitle(paste("Ongoing Indicator Accomplishment (", for_year, ")", sep = "")) +
      theme_minimal() +
      expand_limits(x = nrow(ammended_data) + 1.25, y = 102)

    if (small == FALSE) {

      g <- g +
        geom_point(aes(y = perc_year, shape = "Last Year"), size = 4.5, stroke = 3) +
        scale_shape_manual(" ", values = 124) +
        annotate("text", x = 0, y = ammended_data$percent_time + 1.5,
                 hjust = 0, label = "Today", angle = 90, alpha = 0.5, size = 5) +
        theme(axis.text.y = element_text(size = 15, face = "bold"),
              axis.title.x = element_text(face = "bold", size = 10,
                                          margin = margin(t = 25, r = 0, b = 20, l = 0)),
              axis.text.x = element_text(face = "bold", size = 12),
              title = element_text(face = "bold"),
              plot.title = element_text(hjust = 0.5),
              plot.subtitle = element_text(hjust = 0.5, size = 8))

      if (show_text == TRUE) {
        g <- g +
          geom_text(y = 1, aes(label = tooltip), vjust = -1.5, hjust = 0)
      }

      if (legend == FALSE) {

        g <- g + theme(legend.position = "none")

        print(g)
      } else if (legend == TRUE) {

        print(g)

      }

    } else if (small == TRUE) {

      g <- g +
        geom_point(aes(y = perc_year, shape = "Last Year"), size = 3, stroke = 3) +
        annotate("text", x = 0, y = ammended_data$percent_time + 1.5, hjust = 0, label = "Today",
                 angle = 90, alpha = 0.5, size = 2.5) +
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

      if (show_text == TRUE) {
        warning("When 'small' is set to TRUE, text will not show up by default! \n")
      }

      if (legend == FALSE) {

        g <- g + theme(legend.position = "none")

        print(g)

      } else if (legend == TRUE) {

        print(g)

      }
    }
  } else if (chart_type == "interactive") {

    g <- ggplot(ammended_data, aes(x = indicator_name)) +
      geom_col(aes(y = 100), width = 0.5, alpha = 0.25) +
      geom_bar_interactive(aes(x = indicator_name, y = perc,
                               tooltip = tooltip2,
                               data_id = indicator_name,
                               fill = behind_by),
                           stat = "identity",
                           width = 0.15, color = "black") +
      geom_hline(yintercept = ammended_data$percent_time, alpha = 0.33) +
      scale_fill_gradient("", limits = c(low_level, 0),
                          low = "red", high = "green",
                          guide = FALSE,
                          labels = c("Very Behind Schedule", "Behind Schedule", "Slightly Behind", "On Time"),
                          breaks = c(low_level + 1.5, low_level + 4.15, low_level + 6.25, low_level + 8.5)) +
      coord_flip() +
      labs(y = "Percent of Yearly Target\n&\n Percent of Year",
           x = " ") +
      ggtitle(paste("Ongoing Indicator Accomplishment (", for_year, ")", sep = "")) +
      theme_minimal() +
      expand_limits(x = nrow(ammended_data) + 1.25, y = 102)

    if (small == FALSE) {

      g <- g +
        geom_point(aes(y = perc_year, shape = "Last Year"), size = 4.5, stroke = 3) +
        annotate("text", x = 0, y = ammended_data$percent_time + 1.5,
                 hjust = 0, label = "Today", angle = 90, alpha = 0.5, size = 5) +
        scale_shape_manual(" ", values = 124) +
        theme(axis.text.y = element_text(size = 15, face = "bold"),
              axis.title.x = element_text(face = "bold", size = 10,
                                          margin = margin(t = 25, r = 0, b = 20, l = 0)),
              axis.text.x = element_text(face = "bold", size = 12),
              title = element_text(face = "bold"),
              plot.title = element_text(hjust = 0.5),
              plot.subtitle = element_text(hjust = 0.5, size = 8))

      if (show_text == TRUE) {
        warning("When 'chart_type' is set to 'interactive', text will not show up by default! \n")
      }

      if (legend == FALSE) {

        g <- g + theme(legend.position = "none")

        output <- girafe(code = {print(g)},
                         width_svg = 12
        )
        output

      } else if (legend == TRUE) {

        g <- g + guides(shape = guide_legend(nrow = 1)) + theme(legend.position = "bottom")
        output <- girafe(code = {print(g)},
                         width_svg = 12
        )
        output

      }

    } else if (small == TRUE) {

      g <- g +
        geom_point(aes(y = perc_year, shape = "Last Year"), size = 3, stroke = 3) +
        scale_shape_manual(" ", values = 124) +
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

      if (show_text == TRUE) {
        warning("When 'chart_type' is set to 'interactive', text will not show up by default! \n")
      }

      if (legend == FALSE) {

        g <- g + theme(legend.position = "none")

        output <- girafe(code = {print(g)},
                         width_svg = 20
        )
        output

      } else if (legend == TRUE) {

        g <- g + guides(shape = guide_legend(nrow = 1)) + theme(legend.position = "bottom")
        output <- girafe(code = {print(g)},
                         width_svg = 20
        )
        output

      }
    }
  }
}
