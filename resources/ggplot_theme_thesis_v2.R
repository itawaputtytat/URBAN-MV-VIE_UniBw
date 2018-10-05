theme_thesis <- function(){
  
  theme_bw() + 
  #theme_gray() + 
    
    ## Text
    theme(
      plot.title = 
        element_text(#family = "Helvetica LT Std", 
                     color = "#1b4a7d",
                     size = 9,
                     face = "bold",
                     vjust = 1,
                     margin = margin(t = 0, r = 0, b = 5, l = 0)), 
      plot.subtitle = 
        element_text(#family = "Helvetica LT Std", 
                     vjust = 1,
                     color = "black",
                     size = 8,
                     face = "bold",
                     margin = margin(t = 0, r = 0, b = 5, l = 0)),
      axis.title.x = 
        element_text(#family = "Helvetica LT Std",
                     size = 6,
                     face = "bold",
                     color = "black",
                     #hjust = 0.5),
                     hjust = 0.5),
      #hjust = 0.1), 
      axis.title.y = 
        element_text(#family = "Helvetica LT Std",
                     color = "black",
                     face = "bold",
                     size = 6,
                     hjust = 0.5),
      #hjust = 0.96),
      axis.text = 
        element_text(#family = "Helvetica LT Std",
                     color = "black",
                     size = 6),
    ) +
    
    ## Legend
    theme(
      legend.title = 
        element_text(#family = "Helvetica LT Std",
                     color = "black",
                     size = 6,
                     face = "bold"),
      legend.text = 
        element_text(#family = "Helvetica LT Std",
                     color = "black",
                     size = 6),
      #legend.background = element_rect(fill = "grey98")
      legend.background = element_rect(color = "black", size = 0.15)
    ) +
    
    ## Panel
    theme(
      #panel.grid = element_blank(),
      #panel.background = element_rect(fill = "grey95"),
      panel.background = 
        element_rect(fill = NA),
      panel.border =
        element_rect(#color = "#ced0d1",
                     fill = NA),
      panel.grid.major = 
        element_line(colour = "grey93",
                     size = 0.35),
      panel.grid.minor = 
        element_line(colour = "grey93",
                     size = 0.25)) +
    
    ## Facet
    theme(
      strip.text = element_text(size = 6, 
                                  angle = 0,
                                  face = "bold"),
      strip.background = element_rect(size = 0.5)) +
    
    ## Axis line
    theme(
      axis.line = 
        element_blank(),
      #axis.line = element_line(colour = "#ced0d1"),
      axis.ticks = 
        element_line(color = "grey90", 
                     size = 0.35),
    ) +
    
    theme(
      strip.background = element_rect(fill = convRGB2Hex(47, 84, 150)),
      strip.text = element_text(color = "white")
    )
}


# Backup
# theme_bw() + 
# theme(title = element_text(size = 7, face = "bold")) + 
# theme(axis.title.x = element_text(size = 6),
#       axis.title.y = element_text(size = 6)) + 
# theme(axis.text.x = element_text(size = 6, color = "black"),
#       axis.text.y = element_text(size = 5.5, color = "black")) + 
# theme(legend.key.size = unit(0.2, "cm"),
#       legend.margin = margin(rep(0.1, 4), unit='cm'),
#       legend.title = element_text(size = 6, color = "black"),
#       legend.text = element_text(size = 6, color = "black")) + 
# theme(
#   #panel.grid = element_blank(),
#   #panel.background = element_rect(fill = "grey95"),
#   panel.background = 
#     element_rect(fill = NA),
#   # panel.border = 
#   #   element_rect(color = "#ced0d1", 
#   #                fill = NA),
#   panel.grid.major = 
#     element_line(colour = "grey93",
#                  size = 0.35),
#   panel.grid.minor = 
#     element_line(colour = "grey93",
#                  size = 0.25)
# )
