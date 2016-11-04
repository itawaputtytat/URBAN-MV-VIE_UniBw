intrpldf_batch <- function(namelist = findObjNames("can_s", ".intrpl"),
                           colname4ref = "sxx_dist_m_rnd1",
                           stepsize = 0.1,
                           suffix = ".intrpl") {

  outputFunProc(R)

  invisible( sapply(namelist, function(name) {

    outputString(paste("* Processing object:", name))
    
    ## Get data, unique ids and unique rounds
    dat <- get(name, env = .GlobalEnv)
    subids <- unique(dat[, "subid"])
    rounds <- unique(dat[, "round_txt"])
    
    ## Interpolate data for each round
    dat.intrpl <- invisible( lapply(rounds, function(r) {
      
      #outputString(paste("** Processing round:", r))
      
      filter4r <- paste("round_txt == \"", r, "\"", sep = "")
      
      dat.intrpl_rounds <- invisible( lapply(subids, function(s) {
        
        #outputString(paste("** Processing subject:", s))
        
        filter4rs <- paste(filter4r, "&", "subid ==", s)
        #outputString(filter4rs)
        dat.intrpl_rs <- dat %>% filter_(filter4rs)
        dat.intrpl_rs <- intrpldf(dat.intrpl_rs, colname4ref, stepsize = stepsize)
        return(dat.intrpl_rs)
        
      }) ) %>% dplyr::bind_rows()
      
      return(dat.intrpl_rounds)
      
    }) ) %>% dplyr::bind_rows()

    objname <- name
    objname <- paste(objname, ".intrpl", sep = "")
    assign(objname, dat.intrpl, env = .GlobalEnv)
  }) )

  outputDone()
}
