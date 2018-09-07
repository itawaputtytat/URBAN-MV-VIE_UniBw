# Adjust values for accelaration pedal position by value
correctAccPedalPos <- function(dat, 
                               varname = "acc_pedal_position_perc",
                               varname_suffix = "_corr",
                               correction = 14.901961) {
  
  outputFunProc(R)
  ptm <- proc.time()
  
  # dfnames <- ls(env = .GlobalEnv)[which(grepl(dfname_prefix,
  #                                             ls(env = .GlobalEnv)) == T)]
  
  varname_corr <- paste0(varname, varname_suffix)
  # Loop through data names
  #for(dfname in dfnames) {
  
  name4obj <- paste(deparse(substitute(dat)))
    
    outputString(paste("* Current processing:", name4obj, "... "), linebreak = F)
    
    ## Get data
    #dat <- get(dfname)
    
    # Check if data frame already has corrected value
    #column_checker <- which(names(get(dfname)) == varname_corr)
    #print(length(column_checker))
    # Add variable with corrected values
    if (!varname_corr %in% colnames(dat)) {
      
      colnames_new <- c(colnames(dat), varname_corr)
      
      ## Correct data
      dat <- cbind(dat, dat[, varname] - correction)
      colnames(dat) <- colnames_new
      
      ## Save data
      assign(name4obj, dat, env = .GlobalEnv)
      
    } else {
      #if column checker
      outputString(paste("**", varname, "already corrected"))
    }
  #} #dfname
    outputDone(T)
    outputString(paste("** New column:", varname_corr))  
    
  
  outputProcTime(ptm)
}
