# bullet plot Version 1: actual Stephen FEW  -------------------------------------------------

#' @title bullet_chart
#' @description create a Stephen Few bullet chart
#' @param file_name path of Excel file
#' @param sheet_name Specify which sheet in Excel file, Default: "Sheet1"
#' @param dataframe Specify R dataframe input
#' @param indicator_name Specify the name of the column that has your indicator/KPI names
#' @param actual Specify the name of the column that has the current value of your indicators/KPIs
#' @param actual_lastweek Specify the name of the column that has the indicator/KPI value from the previous week
#' @param actual_lastyear Specify the name of the column that has the indicator/KPI value from the previous year
#' @param target Specify the name of the column that has the target value for the indicator/KPI
#' @param for_year Specify the year in which the report is being made, Default: year(Sys.Date())
#' @param cal_type Define what calendar you are using. Options are "fis" for fiscal year starting
#' October 1st, "cal" for calendar year starting January 1st, or enter your own custom date in the
#' format "YYYY/MM/DD", Default: fis
#' @param chart_type Specify a static or interactive (ggiraph) version
#' @param small Specify whether you want the small version of the plot (TRUE or FALSE), Default: FALSE
#' @param legend Specify whether you want to show the legend, Default: FALSE
#' @param remove_no_targets Remove indicators with Targets == NA or 0, Default: FALSE
#' @param show_text Show 'Last Week' & 'Last Year' text, when `small = TRUE` or
#' `chart_type = "interactive"` then no text will be shown by default.
#' @details This version of the bullet chart most closely resembles Stephen Few's design. The single black bar represents
#' the current value of the indicator while the different hue columns represent last week's value (darker hue) and last year's value (lighter hue).
#' @examples
#' load(read_example("df.rda"))
#' bullet_chart(dataframe = df)
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
                         chart_type = "static",
                         small = FALSE, legend = FALSE,
                         remove_no_targets = FALSE,
                         show_text = FALSE) {

  ammended_data <- extra_field_calculator(file_name, sheet_name,
                                          dataframe,
                                          indicator_name, actual,
                                          actual_lastweek, actual_lastyear,
                                          target, for_year, cal_type,
                                          remove_no_targets)

  ## check for Target == 0 in all Targets
  if(all(ammended_data$target == 0)) {
    return(
      "No Non-Zero Targets!"
    )
  }

  ## base plot
  g <- ggplot(ammended_data, aes(x = indicator_name)) +
    geom_col(aes(y = 100), fill = "grey85", width = 0.4) +
    geom_hline(yintercept = ammended_data$percent_time, alpha = 0.33) +
    coord_flip() +
    labs(y = "Percent of Yearly Target\n&\n Percent of Year",
         x = " ") +
    ggtitle(paste("Ongoing Indicator Accomplishment (", for_year, ")", sep = "")) +
    theme_minimal() +
    expand_limits(x = nrow(ammended_data) + 1.25, y = 102)

  # static vs. interactive ----

  if (chart_type == "static") {
    ### static ----
    if (small == FALSE) {

      g <- g +
        ## Last Week
        geom_col(aes(y = perc_week, alpha = "lastweek"), width = 0.4) +
        ## Last Year
        geom_col(aes(y = perc_year, alpha = "lastyear"), width = 0.4) +
        ## Today
        geom_col(aes(y = perc, alpha = "today"),
                 fill = "grey10", width = 0.1, color = "grey10") +
        scale_alpha_manual(name = "",
                           values = c(0.3, 0.6, 0.9),
                           labels = c("lastweek" = "Last Week",
                                      "lastyear" = "Last Year",
                                      "today" = "Today")) +
        geom_col(aes(y = perc), fill = "grey10", width = 0.1, color = "grey10", alpha = 0.9) +
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
          geom_text(y = 1, aes(label = tooltip), vjust = -2, hjust = 0, size = 4)
      }

      if (legend == FALSE) {

        g <- g + theme(legend.position = "none")

        print(g)

      } else if (legend == TRUE) {

        print(g)

      }

    } else if (small == TRUE) {

      g <- g +
        geom_col(aes(y = perc_week, alpha = "lastweek"), width = 0.4) +
        geom_col(aes(y = perc_year, alpha = "lastyear"), width = 0.4) +
        geom_col(aes(y = perc, alpha = "today"),
                 fill = "grey10", width = 0.1, color = "grey10") +
        scale_alpha_manual(name = "",
                           values = c(0.3, 0.6, 0.9),
                           labels = c("lastweek" = "Last Week",
                                      "lastyear" = "Last Year",
                                      "today" = "Today")) +
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
        g
        warning("When 'small' is set to TRUE, text will not show up by default! \n")
      }

      if (legend == FALSE) {

        g <- g + theme(legend.position = "none")

        print(g)

      } else if (legend == TRUE){

        print(g)

      }
    }
  } else if (chart_type == "interactive") {
    ### interactive ----
    if (small == FALSE) {

      g <- g +
        geom_col(aes(y = perc_week, alpha = "lastweek"), width = 0.4) +
        geom_col(aes(y = perc_year, alpha = "lastyear"), width = 0.4) +
        geom_col(aes(y = perc, alpha = "today"),
                 fill = "grey10", width = 0.1, color = "grey10") +
        scale_alpha_manual(name = "",
                           values = c(0.3, 0.6, 0.9),
                           labels = c("lastweek" = "Last Week",
                                      "lastyear" = "Last Year",
                                      "today" = "Today")) +
        geom_bar_interactive(aes(x = indicator_name, y = perc,
                                 tooltip = tooltip2,
                                 data_id = indicator_name),
                             stat = "identity", alpha = 0.9,
                             fill = "grey10",
                             width = 0.1, color = "grey10") +
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
        g
        warning("When 'chart_type' is set to 'interactive', text will not show up by default! \n")
      }

      if (legend == FALSE) {

        g <- g + theme(legend.position = "none")

        output <- girafe(code = {print(g)},
                         width = 0.5)
        output

      } else if (legend == TRUE) {

        g <- g +
          guides(shape = guide_legend(nrow = 1)) +
          theme(legend.position = "bottom")
        output <- girafe(code = {print(g)},
                         width = 0.5)
        output

      }

    } else if (small == TRUE) {

      g <- g +
        geom_col(aes(y = perc_week, alpha = "lastweek"), width = 0.4) +
        geom_col(aes(y = perc_year, alpha = "lastyear"), width = 0.4) +
        geom_col(aes(y = perc, alpha = "today"),
                 fill = "grey10", width = 0.1, color = "grey10") +
        scale_alpha_manual(name = "",
                           values = c(0.3, 0.6, 0.9),
                           labels = c("lastweek" = "Last Week",
                                      "lastyear" = "Last Year",
                                      "today" = "Today")) +
        geom_bar_interactive(aes(x = indicator_name, y = perc,
                                 tooltip = tooltip2,
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

      if (show_text == TRUE) {
        g
        warning("When 'chart_type' is set to 'interactive', text will not show up by default! \n")
      }

      if (legend == FALSE) {

        g <- g + theme(legend.position = "none")

        output <- girafe(code = {print(g)},
                         width = 0.5)
        output

      } else if (legend == TRUE){

        g <- g + guides(shape = guide_legend(nrow = 1)) + theme(legend.position = "bottom")
        output <- girafe(code = {print(g)},
                         width = 0.5)
        output

      }
    }
  }
}
