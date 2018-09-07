plotPedActSeq <- function(dat, 
                          col_name_am,
                          col_name_id,
                          col_name_facet_row = ".",
                          col_name_facet_col = ".",
                          facet_labeller = NULL,
                          sett_plot = sett_plot,
                          fill_nr_to_grey = NULL) {
  
  if (!is.null(fill_nr_to_grey)) {
    sett_plot$fill[fill_nr_to_grey] <- "grey" 
  }

  plot_dat <- 
    ggplot() +
    geom_raster(data = dat,
                aes_string(x = col_name_am,
                           y = col_name_id,
                           fill = "factor(pedal_act)",
                           alpha = "pedal_int_perc^(1/1.5)"))
  
  if (col_name_facet_row != "." | 
      col_name_facet_col != ".") {
    
    if (!is.null(facet_labeller)) {

      plot_dat <- 
        plot_dat + 
        facet_grid(as.formula(paste(col_name_facet_row, "~", col_name_facet_col)), 
                   scales = "free", 
                   space = "free",
                   labeller = facet_labeller)
      
    } else {
      plot_dat <- 
        plot_dat + 
        facet_grid(as.formula(paste(col_name_facet_row, "~", col_name_facet_col)), 
                   scales = "free", 
                   space = "free")
    }
    

  }
  
  if (!is.null(sett_plot)) {
    plot_dat <- 
      plot_dat +
      scale_fill_manual(values = sett_plot$fill) + 
      coord_cartesian(xlim = c(sett_plot$arr_meas_min, 
                               sett_plot$arr_meas_max))
  }
  
  plot_dat <- 
    plot_dat + 
    guides(fill = FALSE,
           alpha = FALSE) +
    scale_x_continuous(expand = c(0, 0)) +
    #scale_y_discrete(expand = c(0, 0)) +
    scale_y_discrete(expand = c(0, 0), breaks = NULL) +
    theme_bw() + 
    theme(panel.grid = element_blank()) + 
    theme(strip.text.y = element_text(angle=0)) 
  
}