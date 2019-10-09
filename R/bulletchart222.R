# bcdata <- tibble::tibble(
#   variable = c("Revenue", "Order Size",
#                "New Customers", "Satisfaction"),
#   target = c(325, 525, 975, 4.5),
#   value = c(365, 310, 1050, 4),
#   bad = c(220, 430, 600, 2.5),
#   good = c(240, 480, 770, 3.25),
#   great = c(300, 505, 1100, 4.5)
# )
#
# bcdata
#
#
# bcdata %>%
#   #filter(variable == "Revenue") %>%
#   pivot_longer(-c(variable, target),
#                names_to = "allvals", values_to = "value") %>%
#   ggplot() +
#   geom_col(aes(x = variable, y = value, fill = allvals)) +
#   scale_y_continuous(limits = c(0, NA)) +
#   coord_flip() +
#   facet_wrap(~variable, scales = "free_x")
#
#
#
# bcdata %>%
#   mutate(value = if_else(value > great, great, value)) %>%
#   ggplot() +
#   ## great
#   geom_col(aes(x = variable, y = great), fill = "grey10") +
#   ## good
#   geom_col(aes(x = variable, y = good), fill = "grey20") +
#   ## bad
#   geom_col(aes(x = variable, y = bad), fill = "grey") +
#   ## present
#   geom_col(aes(x = variable, y = value), fill = "black" ,
#            width = 0.2) +
#   scale_y_continuous(limits = c(0, NA)) +
#   coord_flip() +
#   facet_wrap(~variable, scales = "free_x") +
#   theme(axis.text.y = element_text(size = 15, face = "bold"),
#         axis.title.x = element_text(face = "bold", size = 10,
#                                     margin = margin(t = 25, r = 0, b = 20, l = 0)),
#         axis.text.x = element_text(face = "bold", size = 12),
#         title = element_text(face = "bold"),
#         plot.title = element_text(hjust = 0.5),
#         plot.subtitle = element_text(hjust = 0.5, size = 8))
#
#
#
# bcdata %>%
#   mutate(value = if_else(value > great, great, value)) %>%
#   ggplot() +
#   ## great
#   geom_col(aes(x = 1, y = great), fill = "#dcdcdc") +
#   ## good
#   geom_col(aes(x = 1, y = good), fill = "#c0c0c0") +
#   ## bad
#   geom_col(aes(x = 1, y = bad), fill = "#696969") +  # grey20
#   ## present
#   geom_col(aes(x = 1, y = value), fill = "black" ,
#            width = 0.2) +
#   scale_y_continuous(limits = c(0, NA),
#                      expand = c(0, 0)) +
#   scale_x_continuous(expand = c(0, 0)) +
#   coord_flip() +
#   facet_wrap(~variable, scales = "free_x", ncol = 1) +
#   theme(title = element_text(face = "bold"),
#         plot.title = element_text(hjust = 0.5),
#         plot.subtitle = element_text(hjust = 0.5, size = 8),
#         panel.grid = element_blank(),
#         axis.title.x = element_blank(),
#         axis.text.x = element_text(face = "bold", size = 12),
#         axis.title.y = element_blank(),
#         axis.text.y = element_blank(),
#         axis.ticks.y = element_blank(),
#         strip.text = element_text(face = "bold", size = 14),
#         strip.background = element_rect(fill = "white"))
#
# ## how to add legend
# ## specifying df cols to cols in plot code >>>> preprocessing + rlang
