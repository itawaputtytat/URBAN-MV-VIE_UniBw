predLiebner_getu <- function(j, k, dat4dvm.spread, dist2compare) {
  if (j %in% c(1, 2)) {
    u <- set4sim$v_ms.max[k]
  } else {
    rowf <- which(dat4dvm.spread$dist == dist2compare)
    u <- dat4dvm.spread[rowf, paste("k", k, sep = "")]
  }
  return(as.numeric(u))
}