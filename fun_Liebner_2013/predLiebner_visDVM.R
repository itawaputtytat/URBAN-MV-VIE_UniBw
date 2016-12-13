predLiebner_visDVM <- function(plotdat_passing, dat4dvm.spread, showplot) {
  
  ## Create new plot data as copy from previous plot data
  ## Necessary for including each velocity model
  plotdat <- plotdat_passing
  
  ## For each velocity model
  for(k in names(dat4dvm.spread)[-1]) {
    
    plotdat <-
      plotdat +
      geom_line(data = get(set4dvm$objname4dvm),
                aes_string(x = "dist",
                           y = k),
                #color = "blue",
                size = 1,
                alpha = 0.25) +
      geom_line(data = get(set4dvm$objname4dvm),
                aes_string(x = "dist",
                           y = k),
                #color = "blue",
                size = 1,
                linetype = "dotted")
    
  } ## End of loop through models
  
  if (showplot) plot(plotdat)
  return(plotdat)
}