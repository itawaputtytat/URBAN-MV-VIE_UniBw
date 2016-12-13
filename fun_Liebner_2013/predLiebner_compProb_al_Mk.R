
predLiebner_compProb_al_Mk <- function(dat4prob, 
                                       dat4accmax, 
                                       acclon_ms2.thresh,
                                       varname4id = "passing",
                                       varname4clustgroup = "clustgroup", 
                                       varname4group = NULL,
                                       return_assignments = F,
                                       showplot = F) {
  
  ## Join cluster assignments and max. lon. acceleration
  prob.assignment <- inner_join(dat4prob, dat4accmax)
  
  ## Assign groups by acceleration thresholds
  prob.assignment$a <- 0
  for(x in 1:length(acclon_ms2.thresh) )  {
    
    prob.assignment <-
      prob.assignment %>%
      mutate(a_rnd05 = round(acclon_ms2.max / 0.5) * 0.5) %>% 
      mutate(a = ifelse(#acclon_ms2.max <= acclon_ms2.thresh[1], 
                        a_rnd05 <= acclon_ms2.thresh[1],
                        paste("l", 1, sep = ""),
                        a) ) %>% 
      mutate(a = ifelse(x != 1 & a == 0 & 
                          #acclon_ms2.max <= acclon_ms2.thresh[x + 1],
                          a_rnd05 <= acclon_ms2.thresh[x + 1],
                        paste("l", x, sep = ""),
                        a) ) %>% 
      mutate(a = ifelse(x == length(acclon_ms2.thresh) & 
                          #acclon_ms2.max > 
                          a_rnd05 >=
                            acclon_ms2.thresh[length(acclon_ms2.thresh)],
                        paste("l", x, sep = ""),
                        a) ) 
  }
  
  ## Rename columns
  prob <- 
    prob.assignment %>% 
    select_(.dots = c(varname4id, 
                      varname4group, 
                      M = varname4clustgroup, "a")) %>% 
    mutate(M = paste("k", M, sep = ""))
  
  ## Create complete combinations to join
  k_unique <- unique(prob$M)
  l_unique <- paste("l", 1:length(acclon_ms2.thresh), sep = "")
  
  ## MUST BE EXTENDED TO VIP!!!
  if (!is.null(varname4group)) {
    group_unique <- unique(dat4prob[, varname4group])
    prob.template <-
      expand.grid(M = k_unique, 
                  a = l_unique, 
                  group = group_unique,
                  stringsAsFactors = F)
    names(prob.template) <- c("M", "a", varname4group)
  } else {
    prob.template <- 
      expand.grid(M = k_unique, 
                  a = l_unique, 
                  stringsAsFactors = F)
  }
  
  ## Compute rates
  prob <- 
    prob %>% 
    group_by_(.dots = lapply(c("M", "a", varname4group), as.symbol)) %>% 
    summarise(count = n()) %>% 
    group_by_(.dots = lapply(c("M", varname4group), as.symbol)) %>% 
    mutate(rate = count/sum(count))
  
  prob <-
    left_join(prob.template,
              prob) %>% 
    mutate(count = ifelse(is.na(count), 0, count),
           rate = ifelse(is.na(rate), 0, rate)) 
  
  ## Visualisation
  if (showplot) {

    txt4caption <- 
      paste(
        paste(
          paste("l", c(1:length(acclon_ms2.thresh)), sep = ""),
          "~",
          acclon_ms2.thresh,
          collapse = "; "),
        "m/s²")
    
    plotdat <-
      ggplot() +
      geom_bar(data = prob,
               aes(x = a,
                   y = rate),
               stat = "identity") +
      facet_grid(.~M) +
      scale_y_continuous(expand = c(0, 0)) + 
      coord_cartesian(ylim = c(0,1)) +
      labs(title = "Distribution of max. acc. parameter a",
           subtitle = paste("Rate of passings in each model Mk"
                            , " with \n", txt4caption, sep = ""))
    
    if (!is.null(varname4group))
      plotdat <-
        plotdat + 
        facet_grid(as.formula(paste("M ~", varname4group)))
    
    plot(plotdat)
    
    dat2return <- list(prob = prob, plotdat = plotdat)
  } else {
    dat2return <- list(prob = prob)
  }
      
  if (return_assignments)
    return(append(dat2return, list(prob.assignment = prob.assignment))) else
    return(dat2return)
}
