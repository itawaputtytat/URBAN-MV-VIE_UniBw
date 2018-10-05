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
                     color = "grey40",
                     size = 8,
                     face = "bold",
                     margin = margin(t = 0, r = 0, b = 5, l = 0)),
      axis.title.x = 
        element_text(#family = "Helvetica LT Std",
                     size = 7,
                     color = "grey40",
                     hjust = 0),
      #hjust = 0.1), 
      axis.title.y = 
        element_text(#family = "Helvetica LT Std",
                     color = "grey40",
                     size = 7,
                     hjust = 1),
      #hjust = 0.96),
      axis.text = 
        element_text(#family = "Helvetica LT Std",
                     color = "grey40",
                     size = 6),
    ) +
    
    ## Legend
    theme(
      legend.title = 
        element_text(#family = "Helvetica LT Std",
                     color = "grey40",
                     size = 6,
                     face = "bold"),
      legend.text = 
        element_text(#family = "Helvetica LT Std",
                     color = "grey40",
                     size = 6),
      #legend.background = element_rect(fill = "grey98")
      legend.background = element_rect(color = "grey93", size = 0.15)
    ) +
    
    ## Panel
    theme(
      #panel.grid = element_blank(),
      #panel.background = element_rect(fill = "grey95"),
      panel.background = 
        element_rect(fill = NA),
      panel.border =
        element_rect(color = "#ced0d1",
                     fill = NA),
      panel.grid.major = 
        element_line(colour = "grey93",
                     size = 0.35),
      panel.grid.minor = 
        element_line(colour = "grey93",
                     size = 0.25)
    ) +
    
    ## Axis line
    theme(
      axis.line = 
        element_blank(),
      #axis.line = element_line(colour = "#ced0d1"),
      axis.ticks = 
        element_line(color = "grey90", 
                     size = 0.35),
    ) +
  
}
