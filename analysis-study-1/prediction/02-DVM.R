
# Preparatory settings ----------------------------------------------------

## Settings for data
set4proc <- c()
set4proc$dfname <- "t_adtf_dist_m_rnd1_full.intrpl.cut"
#set4proc$dfname <- "t_adtf_full.cut"
set4proc$varname4dist <- "sxx_dist_m_rnd1"
#set4proc$varname4dist <- "sxx_dist_s"
set4proc$varname4dist_m <- "sxx_dist_m_rnd1"
set4proc$varname4time <- "time_s"
set4proc$varname4group <- "passing"
set4proc$showplot <- T
set4proc$showplot4subject_id <- 1

## Parameters for IDM
set4idm <- c()
set4idm$acclon_ms2.max <- 0.5
set4idm$acclon.exp <- 4
set4idm$speed_ms.u.max <- 60 / 3.6
set4idm$declon_ms2.comf <- 3
set4idm$gap2leadvhcl_m <- 2
set4idm$gap2leadvhcl_s <- 0.8

## Additional parameters necesseray fpr IDM-splitting
## (e.g. section for pedestrian)
set4idm$distlimit <- 11 #old: 12; 11 shows better empirical desired velocites
set4idm$distlimit <- 11 #for left turning



# Subset data -------------------------------------------------------------

dat <- get(set4proc$dfname) 
#dat <- dat %>% filter(subject_id == 2)
#dat <- dat %>% filter(subject_id <= 11 & subject_id != 11)
  


# Re-compute speed --------------------------------------------------------

dat <- 
  dat %>% 
  group_by_(set4proc$varname4group) %>%
  mutate(speed_ms = speed_kmh / 3.6) %>% 
  data.frame()



# Re-compute and smooth acceleration values -------------------------------

# dat <-
#   dat %>%
#   mutate(acclon_ms2 =
#            (speed_ms - lag(speed_ms)) /
#            (time_s - lag(time_s)) )
# ## For plotting (see below)
# mutate(acclon_ms2.comp = 
#          (speed_ms - lag(speed_ms)) / 
#          (time_s - lag(time_s)) ) %>%

## Compare recorded and re-computed acceleration values
# ggplot() +
#   geom_line(data = dat, aes(x = sxx_dist_m, y = acclon_ms2, group = passing), col = "green") +
#   geom_line(data = dat, aes(x = sxx_dist_m, y = acclon_ms2.comp, group = passing), col = "red")

## Compare recorded and re-computed speed values
# dat <-
#   dat %>% 
#   group_by_(set4proc$varname4group) %>% 
#   mutate(speed_ms.comp = speed_ms) %>% 
#   mutate(speed_ms.comp = acclon_ms2.comp * (time_s - lag(time_s)) + lag(speed_ms.comp))
#   # mutate(speed_ms.comp2 = speed_ms.comp + lag(speed_ms.comp))
# ggplot() + 
#   geom_line(data = dat, aes(x = sxx_dist_m, y = speed_ms, group = passing), col = "green") + 
#   geom_line(data = dat, aes(x = sxx_dist_m, y = speed_ms.comp, group = passing), col = "red")

## Smooth acceleration values
dat <- 
  dat %>% 
  group_by(passing) %>% 
  mutate(acclon_ms2 = smoothWithLoess(acclon_ms2, 1/10, 2, T))



# Visualize velocity profiles ---------------------------------------------

plotdat.profiles <-
  ggplot() +
  geom_line(data = 
              dat %>% 
              filter(subject_id %in% set4proc$showplot4subject_id),
            aes_string(x = set4proc$varname4dist_m,
                       y = "speed_ms",
                       group = set4proc$varname4group),
            size = 1
            #,colour = "grey95"
            ) +
  scale_x_continuous(expand = c(0, 0)) + 
  coord_cartesian(xlim = c(-50, 25),
                 ylim = c(  0, 25)) + 
  theme_bw()

if (set4proc$showplot) plot(plotdat.profiles)



# Estimate maximum acceleration after turning -----------------------------

dat4acclon_ms2.est.max <- 
    dat %>%
    filter_(paste(set4proc$varname4dist_m, ">=", set4idm$distlimit)) %>%
    group_by_(set4proc$varname4group) %>%
    
    ## Using maximum values
    mutate(acclon_ms2.max = max(acclon_ms2)) %>% 
    
    ## Using denominator from formula 3 (Liebner et al., 2013)
    mutate(acclon_ms2.est = 
            acclon_ms2 / 
             (-(speed_ms / set4idm$speed_ms.u.max)^set4idm$acclon.exp + 1)
           ) %>%
    
    ## Summarise for each passing
  group_by_(set4proc$varname4group) %>%
  summarise(acclon_ms2.max = max(acclon_ms2.max),
            acclon_ms2.est.max = max(acclon_ms2.est))

## Merge data and maximum acceleration
dat4idm <- left_join(dat, dat4acclon_ms2.est.max)



# Compute desired velocity profiles ---------------------------------------

dat4idm <- 
  dat4idm %>%
  ## As in formula 3 in Liebner et al. (2003)
  mutate(speed_ms.u = 
           ifelse(acclon_ms2.est.max > acclon_ms2,
                  speed_ms / 
                    ( 1 - acclon_ms2 / acclon_ms2.est.max )^
                    (1 / (set4idm$acclon.exp )),
         ## Workaround: 
         ## In case of max. acceleration is greater before distance limit
         ## ... use absolute value
         ## Alternative 1: Missing values will be imputed using loess (linear)
         ## Alternative 2: Use speed_ms / ( 1 - 1 )^(1 / (set4idm$acclon.exp ))
         ## ... which will give linear values similar to alternative 1
         ## Alternative 3: Move distance limit 
         speed_ms / abs( 1 - acclon_ms2 / acclon_ms2.est.max )^(1 / (set4idm$acclon.exp )))
         ## Compare with actual maximum acceleration
         #( 1 - acclon_ms2 / acclon_ms2.max )^(1 / set4idm$acclon.exp)
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
#                  aes(x = sxx_dist_m_rnd1, 
#                      y = speed_ms.u, 
#                      group = passing),
#                  size = 1) + 
#        geom_line(data = dat4idm2, 
#                  aes(x = sxx_dist_m_rnd1, 
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
  group_by_(set4proc$varname4group, set4proc$varname4dist) %>%
  mutate(speed_ms.u.limit = min(speed_ms.u, set4idm$speed_ms.u.max, na.rm = T))



# Re-compute driven distance ----------------------------------------------

dat4idm <- 
  dat4idm %>%
  #arrange_(set4proc$varname4group, set4proc$varname4time) %>% 
  #mutate_(sxx_dist_m.u = paste(set4proc$varname4dist_m)) %>% 
  #mutate_(sxx_dist_m.u = -50) %>% 
  group_by_(set4proc$varname4group) %>% 
  mutate(sxx_dist_m.u = speed_ms.u.limit * (time_s - lag(time_s))) %>% 
  ## To avoid NA when computing cumsum
  mutate(sxx_dist_m.u = ifelse(is.na(sxx_dist_m.u), 0 , sxx_dist_m.u)) %>% 
  mutate(sxx_dist_m.u = cumsum(sxx_dist_m.u) - 55) %>% 
  data.frame()



# Set desired velocity after distance limit -------------------------------

dat4idm <- 
  dat4idm %>% 
  group_by_(set4proc$varname4group) %>% 
  ## Add desired velocity for section after turning
  mutate(speed_ms.u.limit =
           ifelse(sxx_dist_m_rnd1 >= set4idm$distlimit,
           #ifelse(sxx_dist_m.u >= set4idm$distlimit,
                  set4idm$speed_ms.u.max,
                  speed_ms.u.limit)) 



# Visualize synthesized velocity curves -----------------------------------

plotdat.profiles.synth <-
  plotdat.profiles +
  #ggplot() + 
  geom_path(data = 
              dat4idm, #%>% 
              #filter(round_txt == "normal") %>% 
              #filter(subject_id %in% set4proc$showplot4subject_id),
            aes_string(
              x = set4proc$varname4dist_m,
              #x = "sxx_dist_m.u",
              #y = "speed_ms.u",
              y = "speed_ms.u.limit",
              group = set4proc$varname4group),
            size = 1,
            colour = "green3") #+ 
   # scale_x_continuous(expand = c(0, 0)) +
   # coord_cartesian(xlim = c(-50, 25),
   #                 ylim = c(  0, 25)) +
   # theme_bw()
 
if (set4proc$showplot) plot(plotdat.profiles.synth)
