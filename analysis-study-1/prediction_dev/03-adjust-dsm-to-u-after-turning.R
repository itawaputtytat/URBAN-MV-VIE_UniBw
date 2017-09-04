
# Order DSM by speed ------------------------------------------------------

clustcentres_order <- 
  clustcentres %>% 
  filter_(paste(sett_proc$col_name_dist, "==", 
                min(clustcentres[, sett_proc$col_name_dist]))) %>% 
  group_by_(.dots = lapply("clustgroup", as.symbol)) %>%
  summarise_(speed_ms.initial = 
               paste("mean(", 
                     paste(sett_proc$varname4speed, ".avg", sep = ""), 
                     ")") ) %>% 
  arrange(desc(speed_ms.initial)) %>% 
  mutate(order = row_number()) %>% 
  data.frame()




# Set cluster centres to set of maximum u after distance limit ------------

source("fun_Liebner_2013/settings/set4sim.R")

for(i in clustcentres_order$order) {
  clustgroup_current <- clustcentres_order$clustgroup[i]
  rowfinder <- which(clustcentres$clustgroup == clustgroup_current &
                       clustcentres$pxx_dist_m_rnd1 >= sett_synth$distlimit)
  clustcentres$speed_ms.u.limit.avg[rowfinder] <- rev(set4sim$v_ms.max)[i]
}

# 
# dat4idm <- 
#   dat4idm %>% 
#   group_by_(sett_proc$varname4group) %>% 
#   ## Add desired velocity for section after turning
#   mutate_(speed_ms.u.limit =
#             paste("ifelse(", sett_proc$col_name_dist, ">=", sett_synth$distlimit, ",",
#                   #ifelse(pxx_dist_m.u >= sett_synth$distlimit,
#                   set4idm$u.max, ",",
#                   "speed_ms.u.limit", ")")) 



# Visualise general (ungrouped) cluster centres ---------------------------

plotdat.clustcenters <-
  plotdat.clust +
  geom_line(data = clustcentres,
            aes_string(x = sett_proc$col_name_dist,
                       y = paste(sett_proc$varname4speed, ".avg", sep = ""),
                       colour = "as.factor(clustgroup)"),
            size = 2) #+
#scale_colour_manual(values = c("green3", "blue3", "red3", "orange3"))

plot(plotdat.clustcenters)