
# Preparatory settings ----------------------------------------------------

sett_proc <- c()
sett_proc$df_name <- "study1_t_adtf_pxx_full_dti_rnd1_intrpld_cut" 
sett_proc$pxx <- sett_query$pxx
sett_proc$groupby <- c("pxx", "pxx_dti_m_rnd1")
sett_proc$plot <- T
sett_proc$plotdev <- T
sett_proc$plotmap <- T
sett_proc$plotmap_zoom <- 19
sett_proc$smooth_gps$loess_span <- 1/10
sett_proc$smooth_gps$degree <- 1 ## 1 is enough, see s14
sett_proc$row4origin <- 1
#sett_proc$xlim <- c(-50, 50)
#sett_proc$ylim <- c(-50, 50)6
#sett_proc$kwidth <- 75



# Get initial data --------------------------------------------------------

dat <-  
  get(sett_proc$df_name) %>% 
  filter(pxx == sett_proc$pxx)



# Visualise GPS paths -----------------------------------------------------

## Capture visualisation in case of anomalies
sett_proc$gps_lon_range <- median(dat$gps_lon[dat$pxx_dti_m_rnd1 == 0])
sett_proc$gps_lat_range <- median(dat$gps_lat[dat$pxx_dti_m_rnd1 == 0])

plot_gps_paths <- 
  ggplot() + 
  geom_path(data = dat,
            aes(x = gps_lon,
                y = gps_lat,
                group = passing)) + 
  coord_cartesian(xlim = c(sett_proc$gps_lon_range - 0.002,
                           sett_proc$gps_lon_range + 0.002),
                  ylim = c(sett_proc$gps_lat_range - 0.0006,
                           sett_proc$gps_lat_range + 0.0006))

plot(plot_gps_paths)



# Compute median of GPS paths ---------------------------------------------

dat_gps_median <- 
  dat %>% 
  filter(pxx == sett_proc$pxx) %>% 
  group_by_("pxx", "pxx_dti_m_rnd1") %>%
  summarise_(.dots = 
               c(setNames(list(interp(~ median(v), v = as.name("gps_lon"))),
                          "gps_lon_med"),
                 setNames(list(interp(~ median(v), v = as.name("gps_lat"))),
                          "gps_lat_med"))) %>% 
  data.frame()



# Visualise median of GPS paths -------------------------------------------

plot_gps_paths_median <- 
  plot_gps_paths + 
  geom_path(data = dat_gps_median,
            aes(x = gps_lon_med,
                y = gps_lat_med),
            size = 1.5,
            colour = "orange")

plot(plot_gps_paths_median)




# Visualize steering angle ------------------------------------------------

rfilter <- 
  dat %>% 
  filter(round_txt == "stress") %>% 
  distinct(passing) %>% 
  unlist(use.names = F)

map <- getMapImage(17, zoom = 19)

dat_steer_med <- 
  dat %>% 
  group_by(pxx_dti_m_rnd1) %>% 
  summarise(steer_med = median(steer_angle_deg))

ggplot() + 
  geom_line(data = dat,
            aes(x = pxx_dti_m_rnd1,
                y = steer_angle_deg,
                group = passing)) + 
  geom_line(data = dat_steer_med,
            aes(x = pxx_dti_m_rnd1,
                y = steer_med),
            colour = "red",
            size = 2) + 
  geom_line(data = study1_t_adtf_pxx_full_dti_rnd1 %>% 
              filter(passing == "p17_stress_s25"),
            aes(x = pxx_dti_m_rnd1,
                y = steer_angle_deg,
                group = passing),
            colour = "blue") + 
  geom_line(data = study1_t_adtf_pxx_full_dti_rnd1 %>% 
              filter(passing == "p17_stress_s27"),
            aes(x = pxx_dti_m_rnd1,
                y = steer_angle_deg,
                group = passing),
            colour = "blue") + 
  facet_grid(round_txt~.)



# Compute curvature using speed and yaw rate ------------------------------

dat <- 
  dat %>%
  mutate(curvature = yaw_rate_degs/(speed_kmh/3.6),
         curvature_abs = abs(yaw_rate_degs)/(speed_kmh/3.6) )



# Smooth curvature profiles -----------------------------------------------

# dat <- 
#   dat %>%
#   group_by_("passing") %>% 
#   mutate(curvature_rollavg = rollAvg(curvature, k = 5),
#          curvature_abs_rollavg = rollAvg(curvature_abs, k = 5)) %>% 
#   mutate(curvature_loess = 
#            smoothWithLoess(curvature, 
#                            sett_proc$smooth_gps$loess_span, 
#                            sett_proc$smooth_gps$degree)$fitted,
#          curvature_abs_loess = 
#            smoothWithLoess(curvature_abs, 
#                            sett_proc$smooth_gps$loess_span, 
#                            sett_proc$smooth_gps$degree)$fitted)



# Visualise curvature profiles --------------------------------------------

plot_curvature <- 
  ggplot() + 
  geom_line(data = dat,
            aes(x = pxx_dti_m_rnd1,
                y = curvature_abs,
                group  = passing),
            colour = "grey") +
  theme_bw()

plot(plot_curvature)



# Compute median of curvature profiles ------------------------------------

dat_curvature_median <- 
  dat %>% 
  group_by_("pxx_dti_m_rnd1") %>% 
  #group_by_("round_txt", "pxx_dti_m_rnd1") %>% 
  summarise_(.dots = 
               c(setNames(list(interp(~ median(v), v = as.name("curvature_abs"))),
                          "curvature_abs_median"))) %>% 
                 # setNames(list(interp(~ median(v), v = as.name("curvature_abs_rollavg"))),
                 #          "curvature_rollavg_median"),
                 # setNames(list(interp(~ median(v), v = as.name("curvature_abs_loess"))),
                 #          "curvature_loess_median"))) %>% 
  # mutate(curvature_abs_median_loess = 
  #          smoothWithLoess(curvature_abs_median, 
  #                          0.1, 
  #                          2)$fitted) %>% 
  ungroup() %>%
  mutate(curvature_abs_median_spline = 
           smooth.spline(pxx_dti_m_rnd1, 
                         curvature_abs_median)$y,
         method = ) %>% #"fmm", "natural", "periodic", "monoH.FC" and "hyman"
  data.frame()



# Visualise median of curvature profiles ----------------------------------

plot_curvature_median <- 
  plot_curvature + 
  #ggplot() +
  geom_line(data = dat_curvature_median,
            aes(x = pxx_dti_m_rnd1,
                y = curvature_abs_median),
            size = 1.5,
            colour = "orange") + 
  # geom_line(data = dat_curvature_median,
  #           aes(x = pxx_dti_m_rnd1,
  #               y = curvature_rollavg_median),
  #           size = 1.5,
  #           colour = "green3") +
  # geom_line(data = dat_curvature_median,
  #           aes(x = pxx_dti_m_rnd1,
  #               y = curvature_loess_median),
  #           size = 1.5,
  #           colour = "deepskyblue") + 
  # geom_line(data = dat_curvature_median,
  #           aes(x = pxx_dti_m_rnd1,
  #               y = curvature_abs_median_loess),
  #           size = 1.5,
  #           colour = "red") + 
  geom_line(data = dat_curvature_median,
            aes(x = pxx_dti_m_rnd1,
                y = curvature_abs_median_spline),
            size = 1.5,
            colour = "blue")


plot(plot_curvature_median)



# Integrate curvature to approximate intersection angel -------------------


# Integrate ---------------------------------------------------------------

## Fit cubic smoothing spline to approximate function for curvature profile
curvature_spline_model <- 
  smooth.spline(dat_curvature_median$pxx_dti_m_rnd1, 
                dat_curvature_median$curvature_abs_median)

## Obtain function of smoothing model
curvature_spline_model$fun <- splinefun(curvature_spline_model)

## Integrate 
curvature_spline_model$fun_integration <- 
  integrate(curvature_spline_model$fun, lower = -25, upper = 12)

test_dat <- data.frame(y = test_model$y, x = test_model$x)

coll <- c()
for(p in unique(dat$passing)) {
  
  ## Select data
  dat_temp <- 
    dat %>% 
    filter(passing == p) %>% 
    group_by_("pxx_dti_m_rnd1") %>% 
    summarise_(.dots = 
                 c(setNames(list(interp(~ median(v), 
                                        v = as.name("curvature_abs"))),
                            "curvature_abs_median")))
  
  ## Create model
  model_temp <- 
    smooth.spline(dat_temp$pxx_dti_m_rnd1, 
                  dat_temp$curvature_abs_median)
  
  ## Obtain function of smoothing model
  model_temp$fun <- splinefun(model_temp)
  
  ## Integrate 
  model_temp$fun_integration <- 
    integrate(model_temp$fun, lower = -25, upper = 35)
  
  coll <- rbind(coll, data.frame(passing = p,
                                 integral = model_temp$fun_integration$value))
  
  outputString(paste("* Processing:", p))
  
}
# 
# acos((A-O) %*% (B-O)) * 180 / pi 
# 
# A <- c(48.091448, 11.654095)
# O <- c(48.091442, 11.654265)
# B <- c(48.091528, 11.654366)
# 
# acos((A-O) %*% (B-O)) * 180 / pi 
# 
# datcurv <- 
#   data.frame(x = c(48.091448, 48.091442, 48.091528),
#              y = c(11.654095, 11.654265, 11.654366))
# 
# datcurv_result <- circum(datcurv$x, datcurv$y)
# 
# arccos()
# 



## Kreuzung-Kurvature ableiten aus digitalen Karten
## Wie Kreuzung klassifizieren zur Erkennung von Fahrprofil aus historischen Daten?
## Historische Abbiege-Manöver klassifizieren nach Kurvigkeit
## Aggegrerien auf Winkel
## Winkel integriert aus Curvature

## Welche weiteren Merkmale lassen sich hinzufügen?
## Welche Abhängigkeiten des Geschwindigkeitsprofils gibt es dazu?
## von der Curvature
## von der Visibility
## Mehr-Spurigkeit
## Links und Rechts
