
# Preparatory settings ----------------------------------------------------

## Settings for data
sett_proc <- c()
sett_proc$df_name <- "study1_t_adtf_full_pxx_dist_m_rnd1_intrpld_cut"
sett_proc$col_name_dist <- "pxx_dist_m_rnd1"
sett_proc$col_name_dist_m <- "pxx_dist_m_rnd1"
sett_proc$col_name_time <- "time_s"
sett_proc$col_name_group <- "passing"
sett_proc$pxx <- c(1:15)
sett_proc$show_plot <- F
sett_proc$show_plot_by_subject <- 1

## Parameters for IDM
source("fun_Liebner_2013/settings/set4idm.R")

## Additional parameters necesseray fpr IDM-splitting
## (e.g. section for pedestrian)
sett_synth <- c()
sett_synth$dist_limit <- 11 #old: 12; 11 shows better empirical desired velocites
sett_synth$dist_limit <- 11 #for left turning



# Subset data -------------------------------------------------------------

dat <- 
  get(sett_proc$df_name) %>% 
  filter(pxx %in% sett_proc$pxx) %>% 
  filter(stopping == "no_stopping")
#dat <- dat %>% filter(subject_id == 2)
#dat <- dat %>% filter(subject_id <= 11 & subject_id != 11)
  


# Re-compute speed --------------------------------------------------------

dat <- 
  dat %>% 
  group_by_(sett_proc$col_name_group) %>%
  mutate(speed_ms = speed_kmh / 3.6) %>% 
  data.frame()



# Re-compute and smooth acceleration values -------------------------------

# dat <-
#   dat %>%
#   mutate(acc_lon_ms2 =
#            (speed_ms - lag(speed_ms)) /
#            (time_s - lag(time_s)) )
# ## For plotting (see below)
# mutate(acc_lon_ms2.comp = 
#          (speed_ms - lag(speed_ms)) / 
#          (time_s - lag(time_s)) ) %>%

## Compare recorded and re-computed acceleration values
# ggplot() +
#   geom_line(data = dat, aes(x = pxx_dist_m, y = acc_lon_ms2, group = passing), col = "green") +
#   geom_line(data = dat, aes(x = pxx_dist_m, y = acc_lon_ms2.comp, group = passing), col = "red")

## Compare recorded and re-computed speed values
# dat <-
#   dat %>% 
#   group_by_(sett_proc$col_name_group) %>% 
#   mutate(speed_ms.comp = speed_ms) %>% 
#   mutate(speed_ms.comp = acc_lon_ms2.comp * (time_s - lag(time_s)) + lag(speed_ms.comp))
#   # mutate(speed_ms.comp2 = speed_ms.comp + lag(speed_ms.comp))
# ggplot() + 
#   geom_line(data = dat, aes(x = pxx_dist_m, y = speed_ms, group = passing), col = "green") + 
#   geom_line(data = dat, aes(x = pxx_dist_m, y = speed_ms.comp, group = passing), col = "red")

## Smooth acceleration values
dat <- 
  dat %>% 
  group_by(passing) %>% 
  mutate(acc_lon_ms2 = smoothWithLoess(acc_lon_ms2, 1/10, 2, T))



# Visualize velocity profiles ---------------------------------------------

plotdat.profiles <-
  ggplot() +
  geom_line(data = dat,
            aes_string(x = sett_proc$col_name_dist_m,
                       y = "speed_ms",
                       group = sett_proc$col_name_group),
            size = 1,
            colour = "grey85"
            ) +
  scale_x_continuous(expand = c(0, 0)) + 
  # coord_cartesian(xlim = c(-50, 25),
  #                ylim = c(  0, 25)) + 
  theme_bw()

if (sett_proc$show_plot) plot(plotdat.profiles)



# Estimate maximum acceleration after turning -----------------------------

dat4acc_lon_ms2.est.max <- 
    dat %>%
    filter_(paste(sett_proc$col_name_dist_m, ">=", sett_synth$dist_limit)) %>%
    group_by_(sett_proc$col_name_group) %>%
    
    ## Using maximum values
    mutate(acc_lon_ms2.max = max(acc_lon_ms2)) %>% 
    
    ## Using denominator from formula 3 (Liebner et al., 2013)
    mutate(acc_lon_ms2.est = 
            acc_lon_ms2 / 
             (-(speed_ms / set4idm$u.max)^set4idm$delta + 1)
           ) %>%
    
    ## Summarise for each passing
  group_by_(sett_proc$col_name_group) %>%
  summarise(acc_lon_ms2.max = max(acc_lon_ms2.max),
            acc_lon_ms2.est.max = max(acc_lon_ms2.est))

## Merge data and maximum acceleration
dat4idm <- left_join(dat, dat4acc_lon_ms2.est.max)



# Compute desired velocity profiles ---------------------------------------

dat4idm <- 
  dat4idm %>%
  ## As in formula 3 in Liebner et al. (2003)
  mutate(speed_ms.u = 
           ifelse(acc_lon_ms2.est.max > acc_lon_ms2,
                  speed_ms / 
                    ( 1 - acc_lon_ms2 / acc_lon_ms2.est.max )^
                    (1 / (set4idm$delta )),
         ## Workaround: 
         ## In case of max. acceleration is greater before distance limit
         ## ... use absolute value
         ## Alternative 1: Missing values will be imputed using loess (linear)
         ## Alternative 2: Use speed_ms / ( 1 - 1 )^(1 / (set4idm$delta ))
         ## ... which will give linear values similar to alternative 1
         ## Alternative 3: Move distance limit 
         speed_ms / abs( 1 - acc_lon_ms2 / acc_lon_ms2.est.max )^(1 / (set4idm$delta )))
         ## Compare with actual maximum acceleration
         #( 1 - acc_lon_ms2 / acc_lon_ms2.max )^(1 / set4idm$delta)
  ) %>% 
  data.frame()



# Smooth DVM curves -------------------------------------------------------

# dat4idm2 <-
#   dat4idm %>%
#   group_by(passing) %>%
#   mutate(speed_ms.u = smoothWithLoess(speed_ms.u, 1/75, 2, T))
# 
# plot(ggplot() + 
#        geom_line(data = dat4idm, 
#                  aes(x = pxx_dist_m_rnd1, 
#                      y = speed_ms.u, 
#                      group = passing),
#                  size = 1) + 
#        geom_line(data = dat4idm2, 
#                  aes(x = pxx_dist_m_rnd1, 
#                      y = speed_ms.u, 
#                      group = passing),
#                  col = "red",
#                  size = 0.75) + 
#        
#        coord_cartesian(xlim = c(-50, 25), 
#                        ylim = c(0, 25)) + 
#        theme_bw() + 
#        scale_x_continuous(expand = c(0,0)))



# Limit DVM to maximum desired velocity  ----------------------------------

## Assuming u = umax (see Liebner et al., 2013)
dat4idm <-
  dat4idm %>%
  group_by_(sett_proc$col_name_group, sett_proc$col_name_dist) %>%
  mutate(speed_ms.u.limit = min(speed_ms.u, set4idm$u.max, na.rm = T))



# Re-compute driven distance ----------------------------------------------

dat4idm <- 
  dat4idm %>%
  #arrange_(sett_proc$col_name_group, sett_proc$col_name_time) %>% 
  #mutate_(pxx_dist_m.u = paste(sett_proc$col_name_dist_m)) %>% 
  #mutate_(pxx_dist_m.u = -50) %>% 
  group_by_(sett_proc$col_name_group) %>% 
  mutate(pxx_dist_m.u = speed_ms.u.limit * (time_s - lag(time_s))) %>% 
  ## To avoid NA when computing cumsum
  mutate(pxx_dist_m.u = ifelse(is.na(pxx_dist_m.u), 0 , pxx_dist_m.u)) %>% 
  mutate(pxx_dist_m.u = cumsum(pxx_dist_m.u) - 55) %>% 
  data.frame()



# Set speed to maximum u after distance limit -----------------------------

dat4idm <- 
  dat4idm %>% 
  group_by_(sett_proc$col_name_group) %>% 
  ## Add desired velocity for section after turning
  mutate_(speed_ms.u.limit =
           paste("ifelse(", sett_proc$col_name_dist_m, ">=", sett_synth$dist_limit, ",",
           #ifelse(pxx_dist_m.u >= sett_synth$dist_limit,
                  set4idm$u.max, ",",
                  "speed_ms.u.limit", ")")) 



# Visualize synthesized velocity curves -----------------------------------

plotdat.profiles.synth <-
  plotdat.profiles +
  #ggplot() + 
  geom_path(data = 
              dat4idm, #%>% 
              #filter(round_txt == "normal") %>% 
              #filter(subject_id %in% sett_proc$show_plot_by_subject),
            aes_string(
              x = sett_proc$col_name_dist_m,
              #x = "pxx_dist_m.u",
              #y = "speed_ms.u",
              y = "speed_ms.u.limit",
              group = sett_proc$col_name_group),
            size = 1,
            colour = "green3") #+ 
   # scale_x_continuous(expand = c(0, 0)) +
   # coord_cartesian(xlim = c(-50, 25),
   #                 ylim = c(  0, 25)) +
   # theme_bw()
 
if (sett_proc$show_plot) plot(plotdat.profiles.synth)
