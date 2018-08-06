
# Preparatory settings ----------------------------------------------------

sett_dsm <- c()
sett_dsm$am_limit <- sett_synth$am_limit
sett_dsm$v_kmh_max <- c(48, 54, 60)
sett_dsm$v_ms_max <- sett_dsm$v_kmh_max / 3.6
sett_dsm$show_plot <- T



# Set cluster centres to set of maximum u after distance limit ------------

source("fun_Liebner_2013/settings/sett_sim.R")

dat_clust_centers <- 
  dat_clust_centers %>% 
  mutate(speed_ms_u_smoothed_limitted_avg = 
           ifelse(dti_m_rnd1 >= sett_dsm$am_limit,
                  rev(sett_dsm$v_ms_max)[cluster_group],
                  speed_ms_u_smoothed_avg))
  


# Visualise general (ungrouped) cluster centres ---------------------------

plot_clustcenters_adjusted <-
  plot_clustcenters_ordered +
  geom_line(data = dat_clust_centers,
            aes_string(x = sett_clust$col_name_am,
                       y = paste_("speed_ms_u_smoothed_limitted", "avg"),
                       colour = "factor(cluster_group)"),
            size = 2) #+
#scale_colour_manual(values = c("green3", "blue3", "red3", "orange3"))

if (sett_dsm$show_plot)
  plot(plot_clustcenters_adjusted)
