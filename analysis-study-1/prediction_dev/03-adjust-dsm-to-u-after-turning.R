
# Order DSM by speed ------------------------------------------------------

clustcentres_order <- 
  clustcentres %>% 
  filter_(paste(set4proc$varname4dist_m, "==", 
                min(clustcentres[, set4proc$varname4dist_m]))) %>% 
  group_by_(.dots = lapply("clustgroup", as.symbol)) %>%
  summarise_(speed_ms.initial = 
               paste("mean(", 
                     paste(set4proc$varname4speed, ".avg", sep = ""), 
                     ")") ) %>% 
  arrange(desc(speed_ms.initial)) %>% 
  mutate(order = row_number()) %>% 
  data.frame()




# Set cluster centres to set of maximum u after distance limit ------------

source("fun_Liebner_2013/settings/set4sim.R")

for(i in clustcentres_order$order) {
  clustgroup_current <- clustcentres_order$clustgroup[i]
  rowfinder <- which(clustcentres$clustgroup == clustgroup_current &
                       clustcentres$sxx_dist_m_rnd1 >= set4synth$distlimit)
  clustcentres$speed_ms.u.limit.avg[rowfinder] <- rev(set4sim$v_ms.max)[i]
}

# 
# dat4idm <- 
#   dat4idm %>% 
#   group_by_(set4proc$varname4group) %>% 
#   ## Add desired velocity for section after turning
#   mutate_(speed_ms.u.limit =
#             paste("ifelse(", set4proc$varname4dist_m, ">=", set4synth$distlimit, ",",
#                   #ifelse(sxx_dist_m.u >= set4synth$distlimit,
#                   set4idm$u.max, ",",
#                   "speed_ms.u.limit", ")")) 



# Visualise general (ungrouped) cluster centres ---------------------------

plotdat.clustcenters <-
  plotdat.clust +
  geom_line(data = clustcentres,
            aes_string(x = set4proc$varname4dist_m,
                       y = paste(set4proc$varname4speed, ".avg", sep = ""),
                       colour = "as.factor(clustgroup)"),
            size = 2) #+
#scale_colour_manual(values = c("green3", "blue3", "red3", "orange3"))

plot(plotdat.clustcenters)