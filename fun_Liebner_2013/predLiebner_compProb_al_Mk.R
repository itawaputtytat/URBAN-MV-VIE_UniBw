
predLiebner_compProb_al_Mk <- function(dat, 
                                       col_name_id = "passing",
                                       col_name_cluster_group = "cluster_group", 
                                       col_name_acc_lon_max = "acc_lon_ms2_max",
                                       col_name_group = NULL,
                                       thresholds_acc,
                                       return_assignments = F,
                                       show_plot = F) {
  
  ## Select relevant columns
  dat_prob <- 
    dat %>% 
    distinct_(col_name_id, 
              col_name_cluster_group, 
              col_name_acc_lon_max) 
  
  ## Assign groups based on thresholds
  dat_prob[, "a"] <- NA
  for (i in 1:length(thresholds_acc)) {
    row_finder <- 
      is.na(dat_prob[, "a"]) & dat_prob[, col_name_acc_lon_max] <= 
      thresholds_acc[i]
    dat_prob[row_finder, "a"] <- paste0("l", i)
  }
  
  ## Assign an additional category for values exceeding the last threshold
  row_finder <- is.na(dat_prob[, "a"])
  dat_prob[row_finder, "a"] <- paste0("l", length(thresholds_acc) + 1)
  
  ## Convert a groups to factor to keep levels alive with dply::complete
  temp_levels <- paste0("l", seq(length(thresholds_acc) + 1))
  dat_prob$a <- factor(dat_prob$a, levels = temp_levels)
  
  ## Convert M groups to factor to keep levels alive with dplyr::compelte
  dat_prob[, col_name_cluster_group] <-
    paste0("k", dat_prob[, col_name_cluster_group])
  dat_prob[, col_name_cluster_group] <-
    factor(dat_prob[, col_name_cluster_group])
  
  ## Compute rate per cluster group and threshold
  ## Sort values by M and a 
  dat_prob <- 
    dat_prob %>% 
    group_by_(col_name_cluster_group, "a") %>% 
    mutate(count_per_group = n()) %>% 
    group_by_(col_name_cluster_group) %>% 
    mutate(rate = count_per_group / n()) %>% 
    arrange_(col_name_cluster_group, "a") %>% 
    tidyr::complete_(c(col_name_cluster_group, "a"),
                     fill = list(rate = 0)) %>% 
    distinct_(col_name_cluster_group, "a", "rate")
  
  ## Rename cluster group column
  dat_prob <- 
    dat_prob %>% 
    rename_at(col_name_cluster_group, funs(paste0("M")))
  
  plot_dat <- "No plot data available"
  
  ## Visualisation
  if (show_plot) {
    
    plot_dat <-
      ggplot() +
      geom_bar(data = dat_prob,
               aes_string(x = "a",
                          y = "rate",
                          fill = "M"),
               stat = "identity") +
      facet_grid(.~M, drop = FALSE) +
      scale_x_discrete(drop = FALSE) +
      scale_y_continuous(expand = c(0, 0)) + 
      coord_cartesian(ylim = c(0,1)) +
      labs(title = "Distribution of max. acc. parameter a",
           x = "a (m/s²)",
           y = "P(a | Model k)") + 
      theme_bw()
    
    if (!is.null(col_name_group)) {
      plot_dat <-
        plot_dat + 
        facet_grid(as.formula(paste("M ~", col_name_group)))
    }
    
    plot(plot_dat)
    
  }
  
  
  dat_return <- list(results = dat_prob, plot_dat = plot_dat)
  
  return(dat_return)
}
