# bullet plot Version 1: actual Stephen FEW  -------------------------------------------------

#' @title 'Stephen Few'-style Bullet Chart
#' @description Creates a bullet chart using an indicator's values for the axis scales.
#' @param file_name path of Excel file
#' @param sheet_name specify which sheet in Excel file, Default: "Sheet1"
#' @param dataframe specify R dataframe input
#' @param indicator_name specify the name of the column that has your indicator/KPI names,
#' Default: 'variable'
#' @param info PARAM_DESCRIPTION, Default: 'info'
#' @param current PARAM_DESCRIPTION, Default: 'current'
#' @param low PARAM_DESCRIPTION, Default: 'low'
#' @param medium PARAM_DESCRIPTION, Default: 'medium'
#' @param high PARAM_DESCRIPTION, Default: 'high'
#' @param target PARAM_DESCRIPTION, Default: 'target'
#' @param remove_no_targets PARAM_DESCRIPTION, Default: TRUE
#' @param legend PARAM_DESCRIPTION, Default: TRUE
#' @return bullet chart plot(s)
#' @details Stephen Few style bullet chart
#' @examples
#' data("df_bc")
#' bullet_chart(dataframe = df_bc)
#' @rdname bullet_chart
#' @export
#' @importFrom ggplot2 ggplot geom_col aes geom_segment coord_flip
#' scale_x_continuous scale_y_continuous scale_fill_manual labs theme
#' element_text element_blank element_rect margin
#' @importFrom dplyr filter mutate %>% pull group_by
#' @importFrom purrr map map2
#' @importFrom cowplot get_legend plot_grid
#' @importFrom ggplotify as.ggplot
#' @importFrom tidyr nest
#' @importFrom utils head

bullet_chart <- function(file_name = NULL, sheet_name = "Sheet1",
                        dataframe = NULL,
                        indicator_name = "variable",
                        info = "info",
                        current = "current",
                        low = "low",
                        medium = "medium",
                        high = "high",
                        target = "target",
                        remove_no_targets = TRUE,
                        legend = TRUE) {
  ## Transform data bulletchartr:::field_calculator
  ammended_data <- field_calculator(file_name, sheet_name,
                                    dataframe,
                                    indicator_name, info,
                                    current, low, medium, high,
                                    target, remove_no_targets)

  ## check for Target == 0 in all Targets
  if(all(ammended_data$target == 0)) {
    return(
      "No Non-Zero Targets!"
    )
  }

  ## grab the names of all the indicators
  indicator_vector <- ammended_data$indicator_name %>% unique()

  ## bullet chart plotter function
  bc_plotter <- function(data, indicator_name) {

    ## find mid + max
    min.bg <- 0
    max.bg <- max(data %>%
                    filter(allvals == "High") %>% pull(vals))

    ## min max for 6 labels
    ## ex. Min == 1, 2, 3, 4, Max == 5
    sequence1 <- seq(min.bg, max.bg, length.out = 6) %>% signif(2) %>% head(-1)
    seqbreaks <- c(sequence1, max.bg)

    # seq(min.bg, max.bg, length.out = 5) %>% floor()

    ## fill colors
    cols <- c(High = "#dcdcdc", Medium = "#c0c0c0", Low = "#696969",
              Current = "black")

    ## PLOT
    g <- data %>%
      ggplot() +
      ## great
      geom_col(data = data %>% filter(allvals == "High"),
               aes(x = 1, y = vals, fill = allvals)) +
      ## good
      geom_col(data = data %>% filter(allvals == "Medium"),
               aes(x = 1, y = vals, fill = allvals)) +
      ## bad
      geom_col(data = data %>% filter(allvals == "Low"),
               aes(x = 1, y = vals, fill = allvals)) +
      ## current
      geom_col(data = data %>% filter(allvals == "Current"),
               aes(x = 1, y = vals, fill = allvals),
               width = 0.2) +
      ## target
      geom_segment(aes(x = 0.75, xend = 1.25,
                       y = target, yend = target),
                   color = "red", size = 2.5) +
      coord_flip() +
      scale_y_continuous(limits = c(0, NA),
                         expand = c(0, 0),
                         labels = seqbreaks,
                         breaks = seqbreaks) +
      scale_x_continuous(expand = c(0, 0)) +
      scale_fill_manual(values = cols, name = NULL,
                        breaks = c("Current", "High", "Medium", "Low")) +
      ## var_info takes Indicator name AND any extra info provided in
      ## the 'info' variable, all calculated in `field_calculator()`
      labs(title = glue::glue("{data$varinfo}")) +
      theme(title = element_text(face = "bold"),
            plot.title = element_text(hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5, size = 8),
            panel.grid = element_blank(),
            axis.title.x = element_blank(),
            axis.text.x = element_text(face = "bold", size = 12),
            axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            strip.text = element_text(face = "bold", size = 14),
            strip.background = element_rect(fill = "white"),
            plot.margin = margin(1, 1, 1, 1, "cm"),
            legend.position = "bottom",
            legend.direction = "horizontal")

    return(g)
  }

  ## map over each indicator
  # nested_df <- ammended_data %>%
  #   group_by(indicator_name) %>%
  #   nest()

  plots_df <- ammended_data %>%
    group_by(indicator_name) %>%
    nest() %>%
    mutate(plot = map2(data, indicator_name,
                       ~bc_plotter(data = .x, indicator_name = .y)))
  # plots_df$plot[[1]]
  # plots_df$plot[[2]]
  # plots_df$plot[[3]]
  # plots_df$plot[[4]]

  ## legend ONLY onto bottom-most plot... ----
  ## https://wilkelab.org/cowplot/articles/shared_legends.html
  ## take legend from one of the plots
  ## (always from the first plot as that should always exist...)
  with_legend <- cowplot::get_legend(
    plots_df$plot[[1]] + theme(legend.box.margin = margin(0, 0, 0, 10))
  )

  ## turn into ggplot object
  with_legend_gg <- ggplotify::as.ggplot(with_legend)

  ## remove legend on ALL plots
  removeLegend <- function(plot) {
    plot + theme(legend.position = "none")
  }

  plot_noLegend <- plots_df %>%
    mutate(plot = map(plot, ~ removeLegend(.x)))

  if (legend == FALSE) {
    nolegendplots <- cowplot::plot_grid(plotlist = plot_noLegend$plot,
                                        align = "hv", ncol = 1)

    print(nolegendplots)
  }

  #plot_noLegend$plot[[1]]

  ## append legend "plot" to list of all plots without legends!
  bulletList <- c(plot_noLegend$plot, list(with_legend_gg))

  ## PRINT PLOTS!!
  withlegendplots <- cowplot::plot_grid(plotlist = bulletList,
                                        align = "hv", ncol = 1)

  print(withlegendplots)
}












