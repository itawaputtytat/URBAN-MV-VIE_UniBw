predLiebner_compProb_Mk <- function(dat,
                                    col_name_id = "passing",
                                    col_name_cluster_group = "cluster_group", 
                                    col_name_group = NULL,
                                    show_plot = F) {
  
  dat_prob <- 
    dat %>% 
    select_(.dots = 
              c(col_name_id, 
                M = col_name_cluster_group, 
                col_name_group)) %>% 
    #mutate_(M = col_name_cluster_group) %>% 
    mutate(M = paste("k", M, sep = "")) %>% 
    group_by_(.dots = lapply(c(col_name_id, col_name_group, "M"), as.symbol)) %>% 
    summarise() %>% 
    group_by_(.dots = lapply(c(col_name_group, "M"), as.symbol)) %>% 
    summarise(count = n()) %>% 
    mutate(rate = count/sum(count)) %>% 
    data.frame()
  
  plot_dat <- "No plot data available"
  
  ## Visualisation
  if (show_plot) {
    
    plot_dat <-
      ggplot() +
      geom_bar(data = dat_prob,
               aes(x = M,
                   y = rate,
                   fill = M),
               stat = "identity") +
      scale_y_continuous(expand = c(0, 0)) + 
      coord_cartesian(ylim = c(0,1)) + 
      labs(title = "Distribution of model M",
           subtitle = "Rate of passings in each cluster",
           x = "Model",
           y = "P(Model)") + 
      theme_bw()
    
    if (!is.null(col_name_group)) {
      plot_dat <-
        plot_dat + 
        facet_grid(as.formula(paste(".~", col_name_group)))
    }
    
    plot(plot_dat)
    
  }
  
  return(list(results = dat_prob,
              plot_dat = plot_dat))
}