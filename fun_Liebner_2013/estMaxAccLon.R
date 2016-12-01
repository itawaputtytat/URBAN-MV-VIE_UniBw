## Maximum longitudinal acceleration after turning
estMaxAccLon <- function (dat, varname4group, varname4dist, limit4dist) {
  dat <- 
    dat %>% 
    filter_(paste(varname4dist, ">=", distlimit)) %>%
    group_by_(varname4group) %>%
    mutate(acclon_ms2_est = acclon_ms2 /
             ( ( -(speed_ms/set4idm$speed_ms.u.max)^set4idm$acclon.exp ) +1 )
    ) %>%
    group_by_(varname4group) %>%
    summarise(acclon_ms2_est_max = max(acclon_ms2_est))
}