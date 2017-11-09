
## Find positions where steer_angle_deg is max
## Classify passings as outliers when position lies outsie the 99.5 quantile

# Preparatory settings ----------------------------------------------------

sett_dat <- c()
sett_dat$df_name$data <- paste_(sett_query$df_name, "intrpld")
sett_dat$df_name$intersection_attributes <- "t_pxx_intersection_attributes"
#sett_dat$pxx <- sett_query$pxx
sett_dat$pxx <- c(1:18)
sett_dat$col_name_am <- sett_query$col_name_am
sett_dat$col_name_subject <- "subject_id"
sett_dat$col_name_position <- "pxx"
sett_dat$col_name_case <- "passing"
sett_dat$col_name_group <- "round_txt"
sett_dat$col_name_sa <- "steer_angle_deg"
sett_dat$col_name_gps <- c("gps_lon", "gps_lat")

sett_proc <- c()
sett_proc$am_thresholds_sa_max <- c(-20, 25)
sett_proc$am_thresholds_sa_min1 <- c(-50, 50)
sett_proc$am_thresholds_sa_min2 <- c(-50, 50)
sett_proc$am_thresholds_adapt <- c(0, 0, 0)
sett_proc$sa_threshold_min <- 100
sett_proc$z_cut_off <- 1.96
sett_proc$plot <- F
sett_proc$pause <- F



# Prepare data ------------------------------------------------------------

dat_outlier_coll <- c()
dat_max_summary_coll <- c()

for (p in sett_dat$pxx) {
#for (p in 18) {
  
  dat <- 
    get(sett_dat$df_name$data) %>% 
    filter(pxx %in% p) %>% 
    select_(.dots = c(
      sett_dat$col_name_group,
      sett_dat$col_name_case,
      sett_dat$col_name_am,
      sett_dat$col_name_sa,
      sett_dat$col_name_gps))
  
  outputString(p)
  
  dat_outlier_results <- suppressMessages(
    identifySteerAngleOutliers(dat,
                               sett_dat$col_name_am,
                               sett_dat$col_name_case,
                               sett_dat$col_name_group,
                               sett_dat$col_name_sa,
                               sett_proc$am_thresholds_sa_max,
                               sett_proc$am_thresholds_sa_min1,
                               sett_proc$am_thresholds_sa_min2,
                               sett_proc$am_thresholds_adapt,
                               sett_proc$sa_threshold_min,
                               sett_proc$z_cut_off)
  )
  
  
  ## Add position information and collect data
  dat_outlier_results$dat_max_summary[sett_dat$col_name_position] <- p
    
  dat_max_summary_coll <- 
    rbind(dat_max_summary_coll, 
          dat_outlier_results$dat_max_summary)

  dat_outlier_coll <- 
    rbind(dat_outlier_coll,
          dat_outlier_results$dat_outlier)
  

  ## Visualize
  if (sett_proc$plot) {
    
    ## Join data and outlier information
    dat <- 
      left_join(dat, 
                dat_outlier_results$dat_outlier)
    
    ## Plot steering angle and outlier
    plot_outlier <- 
      ggplot() + 
      geom_line(data = dat,
                aes_string(x = sett_dat$col_name_am,
                           y = sett_dat$col_name_sa,
                           group = sett_dat$col_name_case)) +
      geom_line(data =  dat %>% filter(is_outlier),
                aes_string(x = sett_dat$col_name_am,
                           y = sett_dat$col_name_sa,
                           group = sett_dat$col_name_case),
                size = 1,
                color = "red") + 
      ggtitle(paste(sett_dat$col_name_case, p, sep = ": "))
    
    plot(plot_outlier); pauseAndContinue()
    
    ## Plot GPS data of outlier
    map <- getMapImage(18, zoom = 19)
    
    plot_outlier_map <- 
      ggmap(map) +
      geom_path(dat = dat %>% filter(is_outlier),
                aes_string(x = "gps_lon",
                           y = "gps_lat",
                           color = sett_dat$col_name_case))
    
    # plot_outlier_map <- 
    #   plotGPSPath(dat, p, sett_dat$col_name_case, 
    #               dat_outlier_results$dat_outlier %>% 
    #                 filter(is_outlier) %>% 
    #                 distinct(passing) %>% 
    #                 pull())
    
    plot(plot_outlier_map); pauseAndContinue()
    
  }
  
  if (sett_dat$pause) {
    pauseAndContinue()
  }
  
}
print(dat_outlier_coll %>% filter(is_outlier))



# Post-process data -------------------------------------------------------

## Add info on individual cases and reorder columns
dat_outlier_coll <-
  left_join(dat_outlier_coll,
            get(sett_dat$df_name$data) %>%
              select_(sett_dat$col_name_position, 
                      sett_dat$col_name_subject, 
                      sett_dat$col_name_case) %>%
              distinct(),
            by = sett_dat$col_name_case) %>%
  select_(sett_dat$col_name_position,
          sett_dat$col_name_group,
          sett_dat$col_name_subject,
          sett_dat$col_name_case,
          eval("everything()"))
          # paste_(sett_dat$col_name_am, "min"),
          # paste_(sett_dat$col_name_sa, "max"),
          # "outlier_steer_max",
          # "outlier_steer_min1",
          # "outlier_steer_min2",
          # "outlier_sum",
          # "is_outlier")

## Reorder columns
dat_max_summary_coll <- 
  dat_max_summary_coll %>% 
  select_(sett_dat$col_name_position,
          eval("everything()"))



# Write summary to DB -----------------------------------------------------

dbWriteTable(db_conn_6,
             name = "t_steer_angle_outliers",
             value = dat_outlier_coll,
             row.names = F,
             overwrite = T)



# Write outlier info to DB ------------------------------------------------

dbWriteTable(db_conn_6,
             name = "t_steer_angle_max_summary",
             value = dat_max_summary_coll,
             row.names = F,
             overwrite = T)



# Visualization: Post-processing ------------------------------------------

plot_steer <-
  plot_dat +
  coord_cartesian(xlim = c(min(dat[, sett_dat$col_name_am]),
                           max(dat[, sett_dat$col_name_am])),
                  ylim = c(-600, 600)) + 
  guides(colour = F) + 
  facet_grid(.~round_txt) + 
  ggtitle("Original values",
          subtitle = paste("Intersection:", sett_dat$pxx))

plot(plot_steer)
