## Create vector for selection arrival measures in sett_query
createVectorForAM <- function(var_dist) {
  paste0(c(var_dist, ifelse(grepl("dti_m", var_dist), "tti_s", "dti_m")))
}
