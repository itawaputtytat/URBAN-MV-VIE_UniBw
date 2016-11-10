computeRadius_batch <- function(x, y, seqlength, output = T) {
  
  outputFunProc(R)
  
  ## Initialise collector
  coll <- c()
  
  ## Loop through each segment
  for(i in 1:(length(x) - seqlength + 1)) {
    
    ## Compute row number corresponding to sequence length
    rows <- c(i, i + floor(seqlength / 2), i + seqlength - 1)
    
    ## Create dataframe
    dat2proc <- data.frame(x = x[rows], y = y[rows])
    
    ## Compute circumcircle (radius) and collect values
    radius <- circum(dat2proc$x, dat2proc$y)$radius
    coll <- c(coll, radius)
    if (output) 
      outputString(paste("Current rows:", paste(rows, collapse = " | ")))
  }
  
  dummy_values <- rep(max(coll), floor(seqlength/2))
  dat2proc <- c(dummy_values, coll, dummy_values)
  #dat2proc <- c(coll, dummy_values, dummy_values)
  #dat2proc <- coll
  return(dat2proc)
}