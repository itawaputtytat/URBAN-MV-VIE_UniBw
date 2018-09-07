predLiebner_computeIDM <- function(dat,
                                   col_name_speed,
                                   col_name_acc_lon,
                                   col_name_acc_lon_max,
                                   delta,
                                   col_name_u = NULL,
                                   u_max = 60 / 3.6,
                                   set_u_max = TRUE) {
  
  ## Define target column name
  if (is.null(col_name_u)) {
    col_name_u <- paste_(col_name_speed, "u")
  }
  
  ## Define formula <- 
  formula_idm <- "v / (1 - a / a_max)^(1 / delta)"
  formula_idm <- sub("v", col_name_speed, formula_idm)
  formula_idm <- sub("a", col_name_acc_lon, formula_idm)
  formula_idm <- sub("a_max", col_name_acc_lon_max, formula_idm)
  formula_idm <- sub("delta", delta, formula_idm)
  
  dat_idm <- 
    dat %>% 
    mutate_(.dots = setNames(list(eval(formula_idm)), col_name_u))
  
  if (set_u_max) {
    dat_idm <- 
      dat_idm   %>%
      mutate_(.dots = setNames(list(
        interp(~ ifelse(v > u_max, u_max, v),
               v = as.name(col_name_u),
               u_max = u_max)),
        col_name_u))
  }

  dat_idm <- data.frame(dat_idm)
  
  return(dat_idm)
}