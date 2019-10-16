#' @title bulletchart
#' @description FUNCTION_DESCRIPTION
#' @param file_name PARAM_DESCRIPTION, Default: NULL
#' @param sheet_name PARAM_DESCRIPTION, Default: 'Sheet1'
#' @param dataframe PARAM_DESCRIPTION, Default: NULL
#' @param indicator_name PARAM_DESCRIPTION, Default: 'variable'
#' @param info PARAM_DESCRIPTION, Default: 'info'
#' @param current PARAM_DESCRIPTION, Default: 'current'
#' @param bad PARAM_DESCRIPTION, Default: 'bad'
#' @param good PARAM_DESCRIPTION, Default: 'good'
#' @param great PARAM_DESCRIPTION, Default: 'great'
#' @param target PARAM_DESCRIPTION, Default: 'target'
#' @param remove_no_targets PARAM_DESCRIPTION, Default: TRUE
#' @param legend PARAM_DESCRIPTION, Default: FALSE
#' @return OUTPUT_DESCRIPTION
#' @details Stephen Few style bullet chart
#' @rdname bulletchart
#' @export

bulletchart <- function(file_name = NULL, sheet_name = "Sheet1",
                        dataframe = NULL,
                        indicator_name = "variable",
                        info = "info",
                        current = "current",
                        low = "low",
                        medium = "medium",
                        high = "high",
                        target = "target",
                        remove_no_targets = TRUE,
                        legend = FALSE) {
  ## Transform data
  ammended_data <- bulletchartr:::field_calculator(file_name, sheet_name,
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

  ## fill colors
  cols <- c(High = "#dcdcdc", Medium = "#c0c0c0", Low = "#696969",
            Current = "black")

  ## custom breaks
  # int_breaks <- function(x, n = 5) pretty(x, n)[pretty(x, n) %% 1 == 0]
  # breaks_fun <- function(x) {
  #   br_x <- unique(pretty(seq(min(x), max(x))))
  #   br_x <- c(br_x[-end(br_x)], max(x))
  #   }

  ##

  indicator_vector <- ammended_data$indicator_name %>% unique()

  bc_plotter <- function(data, indicator_name) {

    ## find mid + max
    min.bg <- 0
    max.bg <- max(data %>%
                    filter(allvals == "High") %>% pull(vals))
    low.bg <- max(data %>%
                    filter(allvals == "Low") %>% pull(vals))
    med.bg <- max(data %>%
                    filter(allvals == "Medium") %>% pull(vals))

    ## min max for 5 labels
    sequence1 <- seq(min.bg, max.bg, length.out = 5) %>% signif(2) %>% head(-1)
    seqbreaks <- c(sequence1, max.bg)

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
                         labels = seq(min.bg, max.bg, length.out = 5) %>% floor(),
                         breaks = seq(min.bg, max.bg, length.out = 5) %>% floor()) +
      scale_x_continuous(expand = c(0, 0)) +
      scale_fill_manual(values = cols, name = NULL,
                        breaks = c("Current", "High", "Medium", "Low")) +
      labs(title = glue::glue("{indicator_name}")) +
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

  ## map over each
  # nested_df <- ammended_data %>%
  #   group_by(indicator_name) %>%
  #   nest()

  plots_df <- ammended_data %>%
    group_by(indicator_name) %>%
    nest() %>%
    mutate(plot = map2(data, indicator_name,
                       ~bc_plotter(data = .x, indicator_name = .y)))
  plots_df$plot[[1]]
  plots_df$plot[[2]]
  plots_df$plot[[3]]
  plots_df$plot[[4]]


  cowplot::plot_grid(plotlist = plots_df$plot, align = "hv", ncol = 1)

  ## legend ONLY onto bottom-most plot...
  ## https://wilkelab.org/cowplot/articles/shared_legends.html

  ## legend or no?
  if (legend == FALSE) {
    g <- g + theme(legend.position = "none")
    print(g)
  }
  print(g)

}












