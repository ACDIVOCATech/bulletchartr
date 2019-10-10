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
                        bad = "bad",
                        good = "good",
                        great = "great",
                        target = "target",
                        remove_no_targets = TRUE,
                        legend = FALSE) {
  ## Transform data
  ammended_data <- field_calculator(file_name, sheet_name,
                                    dataframe,
                                    indicator_name, info,
                                    current, bad, good, great,
                                    target, remove_no_targets)

  ## check for Target == 0 in all Targets
  if(all(ammended_data$target == 0)) {
    return(
      "No Non-Zero Targets!"
    )
  }

  ## fill colors
  cols <- c(great = "#dcdcdc", good = "#c0c0c0", bad = "#696969",
            current = "black")

  ## PLOT
  g <- ammended_data %>%
    ggplot() +
    ## great
    geom_col(data = ammended_data %>% filter(allvals == "great"),
             aes(x = 1, y = vals, fill = allvals)) +
    ## good
    geom_col(data = ammended_data %>% filter(allvals == "good"),
             aes(x = 1, y = vals, fill = allvals)) +
    ## bad
    geom_col(data = ammended_data %>% filter(allvals == "bad"),
             aes(x = 1, y = vals, fill = allvals)) +
    coord_flip() +
    facet_wrap(~indicator_name, scales = "free_x", ncol = 1) +
    ## current
    geom_col(data = ammended_data %>% filter(allvals == "current"),
             aes(x = 1, y = vals), fill = "black" ,
             width = 0.2) +
    ## target
    geom_segment(aes(x = 0.75, xend = 1.25,
                     y = target, yend = target),
                 color = "red", size = 2) +
    scale_y_continuous(limits = c(0, NA),
                       expand = c(0, 0)) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_fill_manual(values = cols, name = "Values") +
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
          strip.background = element_rect(fill = "white"))

  if (legend == FALSE) {
    g <- g + theme(legend.position = "none")
    print(g)
  }

  print(g)
}












