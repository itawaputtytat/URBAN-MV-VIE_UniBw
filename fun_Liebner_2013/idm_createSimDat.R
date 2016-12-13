idm_createSimDat <- function(indices, varnames, prefix = "sim_", val4start = NULL) {
 
  if(is.null(val4start))
    val4start <- rep(0, length(varnames))
  
  ## Get names of indices
  names4ind <- names(indices)
  
  ## Create sequence of indices
  ind <- lapply(indices, function(x) 1:x)
  
  ## Get all combinations of indices
  ind <- expand.grid(ind)
  
  ## Name all indices with indices names from list
  ind <- t(apply(ind, 1, function(x) paste(names(ind), x, sep = "")))
  colnames(ind) <- names4ind
  
  ## Arrange by j and k
  ind <- as.data.frame(ind) %>% arrange(j, k)
  
  ## Create combindes string for each index combination
  ind <- apply(ind, 1, function(x) paste(x, collapse = "_"))
  varnames <- lapply(varnames, function(x) paste(x, prefix, ind, sep = ""))
  dat4sim <- rep(0, prod(unlist(indices)) * length(varnames))
  dat4sim <- setNames(dat4sim, unlist(varnames))
  dat4sim <- t(as.data.frame(dat4sim, row.names = NULL))
  dat4sim <- as.data.frame(dat4sim)
  
  ## Adjust speed
  colfinder <- grep("speed", colnames(dat4sim))
  dat4sim[1, colfinder] <- val4start[grep("speed", varnames)]
  colfinder <- grep("dist", colnames(dat4sim))
  dat4sim[1, colfinder] <- val4start[grep("dist", varnames)]
  rownames(dat4sim) <- NULL
  
  return(dat4sim)
}

