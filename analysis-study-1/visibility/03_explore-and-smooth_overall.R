
# Preparatory settings ----------------------------------------------------

sett_dat <- c()
sett_dat$db_conn_name <- dbFindConnObj("Study-1")
sett_dat$src_name <- "t_visibility_percentage"
sett_dat$col_names$position = "position_id"
sett_dat$col_names$am <- "pxx_dti_m_rnd1"

sett_proc <- c()
sett_proc$positions <- c(1:2, 4:18)
sett_proc$col_names$value_left <- "width_intersection_center_to_left_perc"
sett_proc$col_names$value_right <- "width_intersection_center_to_right_perc"
sett_proc$poly <- 12



# Load data ---------------------------------------------------------------

dat <- dbGetSrc(sett_dat$db_conn_name, sett_dat$src_name)

dat <- 
  dat %>% 
  select_(.dots = c(unlist(sett_dat$col_names, use.names = F),
                    unlist(sett_proc$col_names, use.names = F))) %>% 
  data.frame()



# Preprocess and smooth data ----------------------------------------------

## Initialise collector for each position
dat_coll <- c()

for (p in sett_proc$positions) {
  
  ## Select relevant data
  dat_temp <- 
    dat %>% 
    filter_(paste(sett_dat$col_names$position, "==", p)) %>% 
    select_(.dots = c(sett_dat$col_names$am, 
                      unlist(sett_proc$col_names, use.names = F)))
  
  row_finder <- which(dat_temp[, sett_proc$col_names$value_left] < 1)
  dat_temp[row_finder, sett_proc$col_names$value_left] <- 0.01
  
  row_finder <- which(dat_temp[, sett_proc$col_names$value_right] < 1)
  dat_temp[row_finder, sett_proc$col_names$value_right] <- 0.01
  
  ## Interpolate values
  dat_temp <- intrpldf(dat_temp, colname4ref = sett_dat$col_names$am)
  
  ## Smooth values
  dat_temp_smooth_left <- 
    fitPercentageCurveGLM(dat_temp, 
           sett_dat$col_names$am, 
           sett_proc$col_names$value_left, 
           sett_proc$poly)
  
  dat_temp_smooth_right <- 
    fitPercentageCurveGLM(dat_temp, 
           sett_dat$col_names$am, 
           sett_proc$col_names$value_right, 
           sett_proc$poly)

  ## Create template for joining smoothed data
  dat_temp <- 
    dat_temp %>% 
    select_(sett_dat$col_names$am)
  
  ## Add position id
  dat_temp[, sett_dat$col_names$position] <- p
  
  ## Join smoothed daata
  dat_temp <- 
    left_join(dat_temp,
              dat_temp_smooth_left,
              by = sett_dat$col_names$am)
  
  dat_temp <- 
    left_join(dat_temp,
              dat_temp_smooth_right,
              by = sett_dat$col_names$am)

  ## Collect results
  dat_coll <- rbind(dat_coll, dat_temp)
  
}

## Reorder columns
dat_coll <- 
  dat_coll %>% 
  select_(.dots = c(unlist(sett_dat$col_names, use.names = F),
                    unlist(sett_proc$col_names, use.names = F)))



# Write to database -------------------------------------------------------

dbWriteTable(get(sett_dat$db_conn_name),
             "t_visibility_percentage_smoothed", 
             dat_coll, 
             row.names = F,
             overwrite = T)



# Visualize visibility profiles -------------------------------------------

plotdat_left <-
  ggplot() +
  geom_line(data = dat_coll,
            aes_string(x = sett_dat$col_names$am,
                       y = sett_proc$col_names$value_left,
                       group = sett_dat$col_names$position),
            colour = "red4",
            size = 1.25) +
  coord_cartesian(ylim = c(0, 105))

plotdat_right <-
  ggplot() +
  geom_line(data = dat_coll,
            aes_string(x = sett_dat$col_names$am,
             y = sett_proc$col_names$value_right,
             group = sett_dat$col_names$position),
            colour = "green4",
            size = 1.25) +
  coord_cartesian(xlim = c(-50, 0),
                  ylim = c(0, 105)) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0))


plot(plotdat_left)
plot(plotdat_right)
