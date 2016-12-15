
# Subset data -------------------------------------------------------------

#set4dat$varname4dist_m <- "sxx_dist_m"
dat4test <-
  #get(set4dat$objname4src) %>%
  t_adtf_dist_m_rnd1_full.intrpl.cut %>%
  select_(set4dat$varname4group,
          "sxx_dist_m_rnd1",
          set4dat$varname4time,
          #set4dat$varname4speed,
          "speed_kmh",
          set4dat$varname4acclon) %>%
  mutate_(speed_ms = paste("speed_kmh", "/3.6")) %>%
  filter_(paste(set4dat$varname4dist_m, ">=", -50, "&",
                set4dat$varname4dist_m, "<=", 25)) %>%
  data.frame()



# Load passing ------------------------------------------------------------

dat4test <-
  dat4test %>% 
  filter_(paste(set4dat$varname4group, "==", 
                paste("\"", set4dat$passing, "\"", sep = ""))) %>% 
  select(sxx_dist_m_rnd1, time_s, speed_ms, acclon_ms2) %>% 
  data.frame()