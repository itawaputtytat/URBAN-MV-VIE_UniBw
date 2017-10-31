
# Preparatory settings ----------------------------------------------------

set_dat <- c()
set_dat$df_name <- "dat_study1_t_glances_pxx_full_dti_rnd1_intrpld_cut"
set_dat$col_name_glance_id <- "glance_dir_code_v2"
set_dat$col_name_glance_dir <- "glance_dir_label_v2"
set_dat$col_name_case_id <- "passing"
set_dat$col_name_am <- "pxx_dti_m_rnd1"

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
  filter(pxx == 1) %>% 
  filter(round_txt == "normal")



# fsdfsdf -----------------------------------------------------------------

dat2 <- 
  get(set_dat$df_name) %>% 
  filter(pxx == 1) %>% 
  filter(round_txt == "normal")

dat2[, set_dat$col_name_glance_id] <- 
  factor(dat2[, set_dat$col_name_glance_id],
         levels = sort(unique(dat2[, set_dat$col_name_glance_id])),
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
                 color = set_dat$col_name_glance_id)) + 
  facet_grid(pxx~round_txt, scales = "free") + 
  scale_color_manual(values = unlist(set_plot$glance$color)) + 
  theme_bw()

plot(plot_seq)



# Compute glance rates ----------------------------------------------------

col_name_act_level = set_dat$col_name_glance_id
col_name_am = set_dat$col_name_am
col_name_case_id = set_dat$col_name_case_id
col_name_time = "time_s"
dat2proc_cum <- dat_ratio_1st_cum


glancerates_temp <-
  #computeGlanceRates(dat2plot_name,
  computeGlanceRates(dat,
                     set_dat$col_name_glance_id,
                     set_dat$col_name_am,
                     set_dat$col_name_case_id)



# fsdfs -------------------------------------------------------------------

glances_rates1st <- glancerates_temp$dat_1st_full
glances_rates <- glancerates_temp$dat_full



plotdat.glances_rates1st <-
  ##plotdat.visibility +
  ggplot() +
  geom_line(data = glances_rates1st,
            aes_string(x = set_dat$col_name_am,
                       y = "act_n_1st_PER_am_min_ON_cases_n__CUM__1"),
            colour = "blue", size = 1.25) +
  # geom_line(data = glances_rates1st,
  #           aes(x = sxx_dist_m_rnd1,
  #               y = n_subid..n_gl1st.dir.distmin__cum__rear),
  #           colour = "darkorange2", size = 1.5) +
  geom_line(data = glances_rates1st,
            aes_string(x = set_dat$col_name_am,
                       y = "act_n_1st_PER_am_min_ON_cases_n__CUM__4"),
            colour = "red3", size = 1.25) +
  geom_line(data = glances_rates1st,
            aes_string(x = set_dat$col_name_am,
                       y = "act_n_1st_PER_am_min_ON_cases_n__CUM__6"),
            colour = "green4", size = 1.25) +
  ggtitle(paste("Evolution of relative amount of glances on all first glances",
                "(now with complete data)",
                #set4plot$select_sxx,
                sep = "\n")) +
  guides(colour = F, fill = F, size = F, alpha = F)

plot(plotdat.glances_rates1st)



plotdat.glances_rates <-
  ggplot() + 
  geom_line(data = glances_rates,
            aes_string(x = set_dat$col_name_am,
                       y = "act_id_PER_act_level_MAX_ON_act_n_CUM__1"),
            colour = "blue", size = 1.25) +
  geom_line(data = glances_rates,
            aes_string(x = set_dat$col_name_am,
                y = "act_id_PER_act_level_MAX_ON_act_n_CUM__3"),
            colour = "darkorange2", size = 1.25) +
  geom_line(data = glances_rates,
            aes_string(x = set_dat$col_name_am,
                       y = "act_id_PER_act_level_MAX_ON_act_n_CUM__4"),
            colour = "red3", size = 1.25) +
  geom_line(data = glances_rates,
            aes_string(x = set_dat$col_name_am,
                       y = "act_id_PER_act_level_MAX_ON_act_n_CUM__6"),
            colour = "green4", size = 1.25) +
  ggtitle(paste("Evolution of relative amount of glances on all glances",
                "(distribution of glance behaviour)",
                "(now with complete data)",
                #set4plot$select_sxx,
                sep = "\n"))

plot(plotdat.glances_rates)