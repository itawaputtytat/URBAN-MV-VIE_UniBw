
## Smooth values using GLM
## Tried rolling average (puttytat::rollAvg), but deprecated
## Previous versions used individual polynomial grades

# Preparatory settings ----------------------------------------------------

sett_dat <- c()
sett_dat$db_conn_name <- dbFindConnObj("Study-1")
sett_dat$src_name <- "t_visibility_perc"
sett_dat$col_names$position = "position_id"
sett_dat$col_names$am <- "pxx_dti_m_rnd1"

sett_proc <- c()
sett_proc$position <- 4
sett_proc$col_names$value_left <- "width_intersection_center_to_left_perc"
sett_proc$col_names$value_right <- "width_intersection_center_to_right_perc"
sett_proc$poly <- 12



# Load data ---------------------------------------------------------------

dat <- dbGetSrc(sett_dat$db_conn_name, sett_dat$src_name)



# Preprocess data ---------------------------------------------------------

dat <- 
  dat %>% 
  filter_(paste(sett_dat$col_names$position, "==", sett_proc$position)) %>% 
  select_(.dots = c(sett_dat$col_names$am, 
                    unlist(sett_proc$col_names, use.names = F))) %>% 
  data.frame()

row_finder <- which(dat[, sett_proc$col_names$value_left] < 1)
dat[row_finder, sett_proc$col_names$value_left] <- 0.01

## Interpolate values
dat_smooth <- intrpldf(dat, colname4ref = sett_dat$col_names$am, stepsize = 0.1)



# Smooth values -----------------------------------------------------------

dat_smooth_left <- 
  fitGLM(dat_smooth, 
         sett_dat$col_names$am, 
         sett_proc$col_names$value_left, 
         sett_proc$poly)

dat_smooth_right <- 
  fitGLM(dat_smooth, 
         sett_dat$col_names$am, 
         sett_proc$col_names$value_right, 
         sett_proc$poly)



# Visualize ---------------------------------------------------------------

plot_visibility <- 
  ggplot() + 
  geom_point(data = dat,
             aes_string(x = sett_dat$col_names$am ,
                        y = sett_proc$col_names$value_left),
             size = 1) +
  geom_line(data = dat_smooth_left,
            aes_string(x = sett_dat$col_names$am ,
                       y = sett_proc$col_names$value_left),
            col = "red",
            size = 1) +
  geom_point(data = dat,
             aes_string(x = sett_dat$col_names$am ,
                        y = sett_proc$col_names$value_right),
             size = 1) +
  geom_line(data = dat_smooth_right,
            aes_string(x = sett_dat$col_names$am ,
                       y = sett_proc$col_names$value_right),
            col = "green4",
            size = 1) +
  coord_cartesian(ylim = c(0, 100))


plot(plot_visibility)
