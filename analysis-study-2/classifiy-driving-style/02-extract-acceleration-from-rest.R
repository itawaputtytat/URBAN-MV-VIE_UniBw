## ToDo
## Markdown for correct accpedalpos_perc


# Prepare data ------------------------------------------------------------

## Get initial data
dat <- t_adtf_full_pxx_dist_s_rnd1_intrpld_cut

## Correct acceleration pedal position
correctAccPedalPos("dat")

## Code pedal activity
codePedalActivity("dat")

## Filter data
dat <- filter(dat, pxx == 1) 
#dat <- filter(dat, pxx_dist_m_rnd1 <= 10)
#dat <- filter(dat, round_txt == "t2_50") 

# ## Align time to start at zero
# dat <- 
#   dat %>% 
#   group_by(passing) %>% 
#   mutate(time_s_aligned = time_s - min(time_s))

## Compute standard deviation over time
dat_acclon_summary <- 
  dat %>% 
  group_by(round_txt, pxx_dist_s_rnd1) %>% 
  summarise(acclon_ms2_mean = mean(acclon_ms2),
            acclon_ms2_sd = sd(acclon_ms2),
            acclon_ms2_median = mean(acclon_ms2))
         


# Visualise longitudinal acceleration profiles ----------------------------

plot_speed <- 
  ggplot() + 
  geom_line(data = dat,
            aes(x = pxx_dist_s_rnd1,
                y = speed_kmh,
                group = subject_id)) + 
  geom_line(data = dat %>% 
              filter(subject_id == 1),
            aes(x = pxx_dist_s_rnd1,
                y = speed_kmh,
                group = subject_id),
            colour = "red") + 
  facet_grid(.~round_txt)

plot_acclon <- 
  ggplot() + 
  geom_line(data = dat,
            aes(x = pxx_dist_s_rnd1,
                y = acclon_ms2,
                group = subject_id)) + 
  coord_cartesian(#xlim = c(0, 1),
                  ylim = c(-5, 10)) + 
  facet_grid(.~round_txt)

plot_acclon_sd <- 
  ggplot() + 
  geom_line(data = dat_acclon_summary,
            aes(x = pxx_dist_s_rnd1,
                y = acclon_ms2_sd)) + 
  coord_cartesian(ylim = c(0, 2)) + 
  facet_grid(.~round_txt)

grid.arrange(plot_speed, plot_acclon, plot_acclon_sd, nrow = 3)







