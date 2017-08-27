intrpldf_batch4rb <- function(dat,
                              colname4ref,
                              stepsize = 0.1,
                              colnames2excl = NULL,
                              binary_vars = NULL,
                              outputFlag = F,
                              suffix = "intrpld") {

  outputFunProc(R)
  colname4ref_finder <- grep(colname4ref, colnames(dat))
  colname4ref <- colnames(dat)[colname4ref_finder]
  dat_intrpld <- invisible( lapply(unique(dat$passing), function(p) {

    if (outputFlag) 
      outputString(paste0("* Interpolating: ", p, " ... "), linebreak = F)
    
    ## Get data and run inteprolation
    dat_temp <- dat %>% filter(passing == p)

    dat_intrpld <- 
      intrpldf(dat_temp, 
               colname4ref = colname4ref,
               stepsize = stepsize,
               colnames2excl = colnames2excl, 
               binary_vars = binary_vars)
    
    if (outputFlag) 
      outputDone(T)
    
    return(dat_intrpld)

  }) ## lapply
  ) %>% dplyr::bind_rows()
  
  name4obj <- paste(deparse(substitute(dat)), suffix, sep = "_")
  assign(name4obj, dat_intrpld, env = .GlobalEnv)
  outputString(paste("* New object:", name4obj))

  outputDone(T)
}
