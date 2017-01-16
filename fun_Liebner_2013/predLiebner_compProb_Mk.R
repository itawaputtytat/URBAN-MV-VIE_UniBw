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
  plotdat <-
    ggplot() +
    geom_bar(data = prob,
             aes(x = M,
                 y = rate,
                 fill = M),
             stat = "identity") +
    scale_y_continuous(expand = c(0, 0)) + 
    scale_fill_manual(values = c("#ED2124", "#6ABD45", "#3953A4")) +
    coord_cartesian(ylim = c(0,1)) + 
    labs(title = "Distribution of model M",
         subtitle = "Rate of passings in each cluster") + 
    theme_bw()

  if (!is.null(varname4group))
    plotdat <-
    plotdat + 
    facet_grid(as.formula(paste(".~", varname4group)))
    
  if (showplot)  
    plot(plotdat)
  
  return(list(prob = prob,
              plotdat = plotdat))
}