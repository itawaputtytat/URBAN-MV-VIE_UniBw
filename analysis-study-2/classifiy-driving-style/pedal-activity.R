
# Prepare data ------------------------------------------------------------

## Get initial data
dat_pedal <- t_adtf_full_pxx_dist_s_rnd1_intrpld_cut

## Re-initialise data
#dat_pedal <- t_adtf_full_rnd1_full.intrpl.cut
dat_pedal <- filter(dat_pedal, pxx == 2) 
dat_pedal <- filter(dat_pedal, pxx_dist_s_rnd1 <= 10)
#dat_pedal <- filter(dat_pedal, round_txt == "t1_50")

## Correct acceleration pedal position
correctAccPedalPos(dat_pedal)

## Code pedal activity
codePedalActivity(dat_pedal)



# Visualise pedal activity ------------------------------------------------

plot_pedal_activity <- 
  ggplot() +
  geom_tile(data = dat_pedal,
            aes(x = pxx_dist_s_rnd1,
                y = subject_id,
                fill = factor(pedal_act),
                alpha = pedal_int)) +
  facet_grid(round_txt ~ ., drop = T) +
  scale_fill_manual(values = c("red", "white", "green")) + 
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(breaks = unique(dat_pedal$subject_id), expand = c(0, 0)) +
  theme_bw()

#grid.draw(plot_pedal_activity)
ggsave("test0.png", plot_pedal_activity)




# Create sequence object --------------------------------------------------

dat_pedal_seq <- 
  createSeqObj(
    #dat_pedal %>% filter(round_txt == "t1_50"), 
    dat_pedal, 
    id_colname = "passing",
    group_colname = "round_txt",
    key_colname = "pxx_dist_s_rnd1",
    value_colname = "pedal_act",
    states = c("braking", "no pedal activity", "accelerating"),
    labels = c("b", "n", "a")
  )

dat_pedal2 <- dat_pedal_seq$dat_ordered



# Visualise ordered pedal sequences ---------------------------------------

dat_pedal2$round_txt <- as.factor(dat_pedal2$round_txt)

#dev.new();
plot_pedal_activity_ordered <-
  ggplot() +
  geom_tile(data = dat_pedal2,
            aes_string(x = "pxx_dist_s_rnd1",
                       y = "passing",
                       fill = "as.factor(pedal_act)",
                       alpha = "pedal_int")) +
  facet_grid(round_txt~., scales = "free", space = "free") + 
  scale_fill_manual(values = c("red3", "white", "green3")) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  theme(panel.background = element_rect(fill = "white")) +
  theme(legend.position = "none",
        panel.grid = element_blank()) + 
  theme_bw()
# ggtitle(varname2plot) +
# geom_vline(xintercept = as.numeric(pos4steerangle_max),
#            linetype = "dashed")

#grid.draw(plot_pedal_activity_ordered)
ggsave("test1.png", plot_pedal_activity_ordered)



# Order sequences by braking activity -------------------------------------

## Get data
dat_pedal_braking <- dat_pedal
#dat_pedal <- dat_pedal %>% filter(pxx == 3 & round_txt == "t1_50")


## Recode acceleration and no pedal activity to "not braking" == zero
dat_pedal_braking$pedal_act_braking <- dat_pedal_braking$pedal_act


## Get braking distances (by creating sequence id for each pedal activity)
dat_braking_dist <-
  dat_pedal_braking %>%
  group_by(passing) %>%
  mutate(row_nr = row_number()) %>% 
  group_by(passing, pedal_act) %>% 
  mutate(pedal_act_id = row_nr - row_number()) %>%
  filter(pedal_act == -1) %>%
  group_by(passing, pedal_act_id) %>% 
  summarise(dist_m_braking = min(pxx_dist_s_rnd1))


## Get starting position of last braking activity
dat_braking_dist_last <- 
  dat_braking_dist %>% 
  group_by(passing) %>% 
  summarise(dist_m_braking_last = max(dist_m_braking))


## Code every pedal activity before this position as zero
dat_pedal_braking <- 
  left_join(dat_pedal_braking,
            dat_braking_dist_last)

dat_pedal_braking <- 
  dat_pedal_braking %>% 
  mutate(pedal_act_braking = 
           ifelse(pxx_dist_s_rnd1 <= dist_m_braking_last & pedal_act == 1,
                  0,
                  pedal_act))


## Create ordered sequences object
dat_braking_seq <- 
  createSeqObj(
    dat_pedal_braking, 
    id_colname = "passing",
    group_colname = "round_txt",
    key_colname = "pxx_dist_s_rnd1",
    value_colname = "pedal_act_braking",
    states = c("braking", "no pedal activity", "accelerating"),
    labels = c("b", "n", "a")
  )

dat_pedal_braking <- dat_braking_seq$dat_ordered



# Visualise sequences ordered by braking activity -------------------------

plot_pedal_braking_ordered <-
  ggplot() +
  geom_tile(data = dat_pedal_braking,
            aes_string(x = "pxx_dist_s_rnd1",
                       y = "passing",
                       fill = "as.factor(pedal_act)",
                       alpha = "pedal_int")) +
  facet_grid(round_txt~., scales = "free", space = "free") + 
  scale_fill_manual(values = c("red3", "white", "green3")) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  theme(panel.background = element_rect(fill = "white")) +
  theme(legend.position = "none",
        panel.grid = element_blank()) + 
  theme_bw()
# ggtitle(varname2plot) +
# geom_vline(xintercept = as.numeric(pos4steerangle_max),
#            linetype = "dashed")

#grid.draw(plot_pedal_braking_ordered)
ggsave("test2.png", plot_pedal_braking_ordered)



## TODO
## ORDER ACCORDING TO END OF ACCELERATION / START OF FLOATING
####### ORDER ACCORDING TO BRAKING ACTIVITY



# 
# cat("Distribution plot of pedal activity \n")
# 
# ## Distribution plot
# plotdat.pedal_act.distr <-
#   ggplot() +
#   geom_bar(data = dat2plot,
#            aes_string(x = "pxx_dist_s_rnd1",
#                       fill = "as.factor(pedal_act)"),
#            position = "fill",
#            binwidth = 0.1) +
#   scale_fill_manual(values = c("red3", "white", "green3")) +
#   theme(panel.background = element_rect(fill = "white"),
#         legend.position = "none")+
#   scale_x_continuous(expand = c(0, 0)) +
#   scale_y_continuous(expand = c(0, 0)) #+
# #geom_vline(xintercept = as.numeric(pos4steerangle_max))
# 
# plot(plotdat.pedal_act.distr)
# 
# 
# 

