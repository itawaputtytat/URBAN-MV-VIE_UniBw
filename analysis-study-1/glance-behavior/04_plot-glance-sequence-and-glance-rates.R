
# Preparatory settings ----------------------------------------------------

sett_dat <- c()
sett_dat$df_name <- "dat_study1_t_glances_pxx_full_dti_rnd1_intrpld_cut"
sett_dat$col_name_act_level <- "glance_dir_level_v2"
sett_dat$col_name_act_label <- "glance_dir_label_v2"
sett_dat$col_name_case <- "passing"
#sett_dat$col_name_am <- "pxx_dti_m_rnd1"
sett_dat$col_name_am <- "pxx_dti_m_rnd1_steer_max"

sett_proc <- c()
sett_proc$position <- 5
sett_proc$do_plot <- T

sett_act <- c()
sett_act$level <- c(0, 1, 2, 3, 4, 6)

sett_plot <- c()
sett_plot$glances$color$undefined <- "black"
sett_plot$glances$color$ahead <- "blue4"
sett_plot$glances$color$other <- "darkgray"
sett_plot$glances$color$rear <- "orange"
sett_plot$glances$color$left <- "red1"
sett_plot$glances$color$right <- "green4"



# Prepare data ------------------------------------------------------------

dat <- 
  get(sett_dat$df_name) %>% 
  filter_(paste(sett_dat$col_name_am, "<= 0")) %>% 
  filter(pxx == sett_proc$position) %>% 
  filter(round_txt == "normal")

dat2 <- dat
dat2[, sett_dat$col_name_act_level] <- 
  factor(dat2[, sett_dat$col_name_act_level],
         #levels = sort(unique(dat2[, sett_dat$col_name_act_level])),
         levels = sett_act$level,
         labels = names(sett_plot$glances$color))

dat2[, sett_dat$col_name_case] <- 
  factor(dat2[, sett_dat$col_name_case])

dat2[, "is_stopping_dist1_m50_dist2_0_speed_5"] <- 
  factor(dat2[, "is_stopping_dist1_m50_dist2_0_speed_5"])



# Plot sequence plot ------------------------------------------------------

#windows()
plot_seq <- 
  ggplot() + 
  geom_point(data = dat2,
             aes_string(x = sett_dat$col_name_am,
                 y = sett_dat$col_name_case,
                 color = sett_dat$col_name_act_level)) + 
  #facet_grid(pxx~round_txt, scales = "free") + 
  facet_grid(is_stopping_dist1_m50_dist2_0_speed_5~round_txt, scales = "free") + 
  scale_color_manual(values = unlist(sett_plot$glances$color)) + 
  theme_bw()

windows(); plot(plot_seq)
#dev.off()



# Compute glance rates ----------------------------------------------------

col_name_act_level = sett_dat$col_name_act_level
col_name_am = sett_dat$col_name_am
col_name_case = sett_dat$col_name_case
col_name_time = "time_s"

glancerates_temp <-
  computeActivityRates(dat,
                       sett_dat$col_name_act_level,
                       sett_dat$col_name_am,
                       sett_dat$col_name_case,
                       unique_level = sett_act$level)


glances_rates1st <- glancerates_temp$dat_full_1st
glances_rates <- glancerates_temp$dat_full


# fsdfs -------------------------------------------------------------------

if (sett_proc$do_plot) {
  
  plotdat.glances_rates1st <-
    ##plotdat.visibility +
    ggplot() +
    geom_line(data = glances_rates1st,
              aes_string(x = sett_dat$col_name_am,
                         y = "act_nr_PER_act_level_1st_ON_cases_n__1"),
              colour = "blue", size = 1.25) +
    # geom_line(data = glances_rates1st,
    #           aes(x = sxx_dist_m_rnd1,
    #               y = n_subid..n_gl1st.dir.distmin__cum__rear),
    #           colour = "darkorange2", size = 1.5) +
    geom_line(data = glances_rates1st,
              aes_string(x = sett_dat$col_name_am,
                         y = "act_nr_PER_act_level_1st_ON_cases_n__4"),
              colour = "red3", size = 1.25) +
    geom_line(data = glances_rates1st,
              aes_string(x = sett_dat$col_name_am,
                         y = "act_nr_PER_act_level_1st_ON_cases_n__6"),
              colour = "green4", size = 1.25) +
    coord_cartesian(ylim = c(0, 100)) +
    ggtitle(paste("Evolution of relative amount of first glances on all first glances in that direction",
                  "(now with complete data)",
                  #set4plot$select_sxx,
                  sep = "\n")) +
    guides(colour = F, fill = F, size = F, alpha = F)
  
  plot(plotdat.glances_rates1st)
  
  
  
  plotdat.glances_rates1st_2 <-
    ##plotdat.visibility +
    ggplot() +
    geom_line(data = glances_rates1st,
              aes_string(x = sett_dat$col_name_am,
                         y = "act_nr_PER_act_level_1st_ON_act_n_PER_act_level__1"),
              colour = "blue", size = 1.25) +
    # geom_line(data = glances_rates1st,
    #           aes(x = sxx_dist_m_rnd1,
    #               y = n_subid..n_gl1st.dir.distmin__cum__rear),
    #           colour = "darkorange2", size = 1.5) +
    geom_line(data = glances_rates1st,
              aes_string(x = sett_dat$col_name_am,
                         y = "act_nr_PER_act_level_1st_ON_act_n_PER_act_level__4"),
              colour = "red3", size = 1.25) +
    geom_line(data = glances_rates1st,
              aes_string(x = sett_dat$col_name_am,
                         y = "act_nr_PER_act_level_1st_ON_act_n_PER_act_level__6"),
              colour = "green4", size = 1.25) +
    coord_cartesian(ylim = c(0, 100)) +
    ggtitle(paste("Evolution of relative amount of first glances on all glances in that direction",
                  "(now with complete data)",
                  #set4plot$select_sxx,
                  sep = "\n")) +
    guides(colour = F, fill = F, size = F, alpha = F) #+ 
    #theme_thesis()
 
  plot(plotdat.glances_rates1st_2)
  
  # ggsave("test1.png", 
  #        plotdat.glances_rates1st_2, 
  #        dpi = 600, 
  #        width = 8, 
  #        height = 6,
  #        units = "cm")
  
  
  
  
  plotdat.glances_rates <-
    ggplot() + 
    geom_line(data = glances_rates,
              aes_string(x = sett_dat$col_name_am,
                         y = "act_nr_PER_act_level_ON_act_n__1"),
              colour = "blue", size = 1.25) +
    geom_line(data = glances_rates,
              aes_string(x = sett_dat$col_name_am,
                         y = "act_nr_PER_act_level_ON_act_n__3"),
              colour = "darkorange2", size = 1.25) +
    geom_line(data = glances_rates,
              aes_string(x = sett_dat$col_name_am,
                         y = "act_nr_PER_act_level_ON_act_n__4"),
              colour = "red3", size = 1.25) +
    geom_line(data = glances_rates,
              aes_string(x = sett_dat$col_name_am,
                         y = "act_nr_PER_act_level_ON_act_n__6"),
              colour = "green4", size = 1.25) +
    coord_cartesian(ylim = c(0, 100)) +
    ggtitle(paste("Evolution of relative amount of glances on all glances",
                  "(distribution of glance behaviour)",
                  "(now with complete data)",
                  #set4plot$select_sxx,
                  sep = "\n")) #+ 
    #theme_thesis()
  
  plot(plotdat.glances_rates)
  
}


