## Create vector for selection arrival measures in sett_query
createVector_var_pxx <- function(var_dist) {
  paste0("_", 
         c(var_dist, 
           ifelse(grepl("dist_m", var_dist), "dist_s", "dist_m")))
}
