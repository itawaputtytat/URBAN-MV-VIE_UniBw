predLiebner_compProb_Mk <- function(dat2proc,
                                    varname4id = "passing",
                                    varname4clustgroup = "clustgroup", 
                                    varname4group = NULL,
                                    showplot = F) {
  
  prob <- 
    dat2proc %>% 
    select_(.dots = c(varname4id, M = varname4clustgroup, varname4group)) %>% 
    #mutate_(M = varname4clustgroup) %>% 
    mutate(M = paste("k", M, sep = "")) %>% 
    group_by_(.dots = lapply(c(varname4id, varname4group, "M"), as.symbol)) %>% 
    summarise() %>% 
    group_by_(.dots = lapply(c(varname4group, "M"), as.symbol)) %>% 
    summarise(count = n()) %>% 
    mutate(rate = count/sum(count)) %>% 
    data.frame()
  
  ## Visualisation
  if (showplot) {
    
    plotdat <-
      ggplot() +
      geom_bar(data = prob,
               aes(x = M,
                   y = rate),
               stat = "identity") +
      scale_y_continuous(expand = c(0, 0)) + 
      coord_cartesian(ylim = c(0,1)) + 
      labs(title = "Distribution of model M",
           subtitle = "Rate of passings in each cluster")

    if (!is.null(varname4group))
      plotdat <-
        plotdat + 
        facet_grid(as.formula(paste(".~", varname4group)))
      
    plot(plotdat)
    
    return(list(prob = prob,
                plotdat = plotdat))
  } else
    ## If no visualisation
    return(list(prob = prob))
}