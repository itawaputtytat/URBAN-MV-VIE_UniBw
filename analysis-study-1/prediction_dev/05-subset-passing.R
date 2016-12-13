source("fun_Liebner_2013/settings/set4dat.R")

dat <-
  #get(set4dat$objname4src) %>%
  t_adtf_dist_m_rnd1_full.intrpl.cut %>%
  select_(set4dat$varname4group,
          set4dat$varname4dist_m,
          set4dat$varname4time,
          #set4dat$varname4speed,
          "speed_kmh",
          set4dat$varname4acclon) %>%
  mutate_(speed_ms = paste("speed_kmh", "/3.6")) %>%
  filter_(paste(set4dat$varname4dist_m, ">=", -50, "&",
                set4dat$varname4dist_m, "<=", 25)) %>%
  data.frame()



# Load passing ------------------------------------------------------------

set4dat$passing <- "s04_intro_subject05"

dat4test <-
  dat %>% 
  filter_(paste(set4dat$varname4group, "==", 
                paste("\"", set4dat$passing, "\"", sep = ""))) %>% 
  select(sxx_dist_m_rnd1, time_s, speed_ms, acclon_ms2) %>% 
  data.frame()



# Compute increase in time and distance -----------------------------------

#dat4test <- compLagDiff(dat4test, c(set4dat$varname4time))