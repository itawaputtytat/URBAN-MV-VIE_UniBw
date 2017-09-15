
# Subset data -------------------------------------------------------------

outputSectionTitle("Loading test data")
outputString(paste("* Source:", sett_dat$df_name))
outputString(paste("* Variable for position:", sett_dat$col_name_am))
outputString(paste("* Variable for spatial distance:", sett_dat$col_name_dist))

dat_test <-
  get(sett_dat$df_name) %>%
  select_(sett_dat$col_name_group,
          sett_dat$col_name_am,
          sett_dat$col_name_time,
          sett_dat$col_name_dist,
          sett_dat$col_name_speed,
          sett_dat$col_name_acc_lon) %>%
  filter_(paste(sett_dat$col_name_am, ">=", sett_dat$am_limit1, "&",
                sett_dat$col_name_am, "<=", sett_dat$am_limit2)) %>%
  data.frame()



# Load passing ------------------------------------------------------------

if (!sett_dat$case %in% unique(dat_test[, sett_dat$col_name_group]))
  stop("Selected passing not found")

dat_test <-
  dat_test %>%
  filter_(paste(sett_dat$col_name_group, "==", deparse(sett_dat$case) )) %>%
  select_(.dots = c(sett_dat$col_name_am,
                    sett_dat$col_name_time, 
                    sett_dat$col_name_dist, 
                    sett_dat$col_name_speed, 
                    sett_dat$col_name_acc_lon)) %>%
  data.frame()