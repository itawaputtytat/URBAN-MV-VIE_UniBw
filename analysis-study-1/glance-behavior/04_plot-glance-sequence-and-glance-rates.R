
# Preparatory settings ----------------------------------------------------

set_dat <- c()
set_dat$df_name <- "dat_study1_t_glances_pxx_full_dti_rnd1_intrpld_cut"
set_dat$col_name_act_level <- "glance_dir_level_v2"
set_dat$col_name_act_label <- "glance_dir_label_v2"
set_dat$col_name_case_id <- "passing"
set_dat$col_name_am <- "pxx_dti_m_rnd1"

set_act <- c()
set_act$level <- c(0, 1, 2, 3, 4, 6)

set_plot <- c()
set_plot$glance$color$undefined <- "black"
set_plot$glance$color$ahead <- "blue4"
set_plot$glance$color$other <- "darkgray"
set_plot$glance$color$rear <- "orange"
set_plot$glance$color$left <- "red1"
set_plot$glance$color$right <- "green4"



# Prepare data ------------------------------------------------------------

dat <- 
  get(set_dat$df_name) %>% 
  filter_(paste(set_dat$col_name_am, "<= 0")) %>% 
  filter(pxx == 5) %>% 
  filter(round_txt == "normal")

dat2 <- dat
dat2[, set_dat$col_name_act_level] <- 
  factor(dat2[, set_dat$col_name_act_level],
         levels = sort(unique(dat2[, set_dat$col_name_act_level])),
         labels = names(set_plot$glance$color))

dat2[, set_dat$col_name_case] <- 
  factor(dat2[, set_dat$col_name_case])



# Plot sequence plot ------------------------------------------------------

#windows()
plot_seq <- 
  ggplot() + 
  geom_point(data = dat2,
             aes_string(x = set_dat$col_name_am,
                 y = set_dat$col_name_case,
                 color = set_dat$col_name_act_level)) + 
  facet_grid(pxx~round_txt, scales = "free") + 
  scale_color_manual(values = unlist(set_plot$glance$color)) + 
  theme_bw()

windows(); plot(plot_seq)
dev.off()



# Compute glance rates ----------------------------------------------------

col_name_act_level = set_dat$col_name_act_level
col_name_am = set_dat$col_name_am
col_name_case_id = set_dat$col_name_case_id
col_name_time = "time_s"

glancerates_temp <-
  computeActivityRates(dat,
                       set_dat$col_name_act_level,
                       set_dat$col_name_am,
                       set_dat$col_name_case_id,
                       unique_level = set_act$level)



# fsdfs -------------------------------------------------------------------

glances_rates1st <- glancerates_temp$dat_full_1st
glances_rates <- glancerates_temp$dat_full

plotdat.glances_rates1st <-
  ##plotdat.visibility +
  ggplot() +
  geom_line(data = glances_rates1st,
            aes_string(x = set_dat$col_name_am,
                       y = "act_nr_PER_act_level_1st_ON_cases_n__1"),
            colour = "blue", size = 1.25) +
  # geom_line(data = glances_rates1st,
  #           aes(x = sxx_dist_m_rnd1,
  #               y = n_subid..n_gl1st.dir.distmin__cum__rear),
  #           colour = "darkorange2", size = 1.5) +
  geom_line(data = glances_rates1st,
            aes_string(x = set_dat$col_name_am,
                       y = "act_nr_PER_act_level_1st_ON_cases_n__4"),
            colour = "red3", size = 1.25) +
  geom_line(data = glances_rates1st,
            aes_string(x = set_dat$col_name_am,
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
            aes_string(x = set_dat$col_name_am,
                       y = "act_nr_PER_act_level_1st_ON_act_n_PER_act_level__1"),
            colour = "blue", size = 1.25) +
  # geom_line(data = glances_rates1st,
  #           aes(x = sxx_dist_m_rnd1,
  #               y = n_subid..n_gl1st.dir.distmin__cum__rear),
  #           colour = "darkorange2", size = 1.5) +
  geom_line(data = glances_rates1st,
            aes_string(x = set_dat$col_name_am,
                       y = "act_nr_PER_act_level_1st_ON_act_n_PER_act_level__4"),
            colour = "red3", size = 1.25) +
  geom_line(data = glances_rates1st,
            aes_string(x = set_dat$col_name_am,
                       y = "act_nr_PER_act_level_1st_ON_act_n_PER_act_level__6"),
            colour = "green4", size = 1.25) +
  coord_cartesian(ylim = c(0, 100)) +
  ggtitle(paste("Evolution of relative amount of first glances on all glances in that direction",
                "(now with complete data)",
                #set4plot$select_sxx,
                sep = "\n")) +
  guides(colour = F, fill = F, size = F, alpha = F)

plot(plotdat.glances_rates1st_2)



plotdat.glances_rates <-
  ggplot() + 
  geom_line(data = glances_rates,
            aes_string(x = set_dat$col_name_am,
                       y = "act_nr_PER_act_level_ON_act_n__1"),
            colour = "blue", size = 1.25) +
  geom_line(data = glances_rates,
            aes_string(x = set_dat$col_name_am,
                y = "act_nr_PER_act_level_ON_act_n__3"),
            colour = "darkorange2", size = 1.25) +
  geom_line(data = glances_rates,
            aes_string(x = set_dat$col_name_am,
                       y = "act_nr_PER_act_level_ON_act_n__4"),
            colour = "red3", size = 1.25) +
  geom_line(data = glances_rates,
            aes_string(x = set_dat$col_name_am,
                       y = "act_nr_PER_act_level_ON_act_n__6"),
            colour = "green4", size = 1.25) +
  coord_cartesian(ylim = c(0, 100)) +
  ggtitle(paste("Evolution of relative amount of glances on all glances",
                "(distribution of glance behaviour)",
                "(now with complete data)",
                #set4plot$select_sxx,
                sep = "\n"))

plot(plotdat.glances_rates)