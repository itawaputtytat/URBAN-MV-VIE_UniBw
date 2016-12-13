compLagDiff <- function(dat2proc, varnames, suffix = ".diff") {
  newvars <- sapply(varnames, function(x) {
    temp <- dat2proc[, x] - lag(dat2proc[, x])
  }) 
  colnames(newvars) <- paste(varnames, suffix, sep = "")
  dat2proc <- cbind(dat2proc, newvars)
  return(dat2proc)
}