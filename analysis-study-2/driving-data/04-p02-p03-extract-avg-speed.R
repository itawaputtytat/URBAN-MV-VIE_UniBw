
# Settings ----------------------------------------------------------------

## Data
sett <- c()
sett$meta$df_name <- "dat_study2_t_adtf_pxx_full_aggr_dti_rnd1_intrpld_cut"
#sett$meta$df_name <- "dat_study2_t_adtf_pxx_full_aggr_tti_rnd1_intrpld_cut"
sett$meta$am <- ifelse(grepl("dti", sett$meta$df_name), "dti", "tti")
sett$meta$am_ger <- ifelse(sett$meta$am == "dti", "dzk", "zzk")

## Filter
sett$filters$pxx <- 2
sett$filters$am <- "dti"
sett$filters$am_lower <- -125
sett$filters$am_upper <- 50
sett$filters$am_lower_precise <- -50
sett$filters$am_upper_precise <- 0

## Column names
sett$col_names$pxx <- c("pxx")
sett$col_names$am <- findColNameForAM(sett$meta$am, get(sett$meta$df_name))
sett$col_names$dti <- findColNameForAM("dti", get(sett$meta$df_name))
sett$col_names$tti <- findColNameForAM("tti", get(sett$meta$df_name))
sett$col_names$measure_speed <- "speed_ms"
sett$col_names$measure_acc_lat <- "acc_lat_ms2"
sett$col_names$measure_acc_lat_abs <- 
  paste_(sett$col_names$measure_acc_lat, "abs")
sett$col_names$id <- "passing"
sett$col_names$subject <- "subject_id"
sett$col_names$condition_run <- "condition_run"
sett$col_names$condition_speed <- "condition_speed"

## Data for experimental conditions
sett$db$db_names$study2 <- "URBAN-MV-VIE_UniBw_Study-2"
sett$db$conn_names$study2 <- dbFindConnObj(sett$db$db_names$study2, output = F)



# Preprocess data ---------------------------------------------------------

## Get data
dat <- get(sett$meta$df_name) 

## Filter position
dat <- 
  dat %>% 
  ## Filter for position
  filter_(createFilterString(sett$col_names$pxx, sett$filters$pxx))

## Filter AM
dat <- 
  dat %>% 
  filter_(paste(sett$col_names$dti, ">=", 
                max(-999, sett$filters$am_lower, na.rm = T))) %>% 
  filter_(paste(sett$col_names$dti, "<=", 
                min(999, sett$filters$am_upper, na.rm = T))) 

## Create new column for absolute acc. lat.
dat[, sett$col_names$measure_acc_lat_abs] <- 
  abs(dat[, sett$col_names$measure_acc_lat])



# Visualize speed profiles ------------------------------------------------

plot_profile <- 
  ggplot() + 
  geom_line(data = dat,
            aes_string(x = sett$col_names$am,
                       y = sett$col_names$measure_speed,
                       group = sett$col_names$id),
            size = 0.2) + 
  facet_grid(as.formula(
    #paste(sett$col_names$pxx, "~", 
    paste(".", "~", 
          sett$col_names$condition_speed))) +
  coord_cartesian(y = c(0, 100),
                  expand = c(0, 0))

windows(); plot(plot_profile)



# Extract average speed while approaching the intersection ----------------

## Compute summary
temp_groups <- 
  c(sett$col_names$id,
    sett$col_names$pxx,
    sett$col_names$subject,
    sett$col_names$condition_speed,
    sett$col_names$condition_run
  )

temp_values <- 
  c(sett$col_names$measure_speed)

dat_speed_summary <- 
  computeSummary(
    dat %>% 
      filter_(paste(sett$col_names$am, ">=", 
                    max(-999, sett$filters$am_lower_precise, na.rm = T))) %>% 
      filter_(paste(sett$col_names$am, "<=", 
                    min(999, sett$filters$am_upper_precise, na.rm = T))),
    col_names_group = temp_groups,
    col_names_values = temp_values)



# Write to database -------------------------------------------------------

## Summary
sett$table_names$summary <- 
  paste_("t_adtf_results",
         sprintf("p%02d", sett$filters$pxx),
         sett$col_names$am,
         "speed_summary")

dbWriteTable(get(sett$db$conn_names$study2),
             name = sett$table_names$summary,
             dat_speed_summary,
             row.names = F,
             overwrite = T)
