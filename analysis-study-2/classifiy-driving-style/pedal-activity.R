## @knitr pedal-activity

# Prepare data ------------------------------------------------------------

## Initialise data
dat_pedal <- t_adtf_full_pxx_dist_m_rnd1_intrpld_cut

## Filter data for situations of interest
dat_pedal <- filter(dat_pedal, pxx %in% c(2,3)) 
dat_pedal <- filter(dat_pedal, pxx_dist_m_rnd1 <= 10)
#dat_pedal <- filter(dat_pedal, round_txt == "t1_50")

## Re-code  pxx and round_txt into new variable 
dat_pedal$pxx_round_txt <- 
  paste(sprintf("p%02d", dat_pedal$pxx), dat_pedal$round_txt, sep = "_")

## Correct acceleration pedal position
correctAccPedalPos(dat_pedal)

## Code pedal activity
codePedalActivity(dat_pedal) 



# Settings for dynamic processing -----------------------------------------

## Set settings for processing
sett_proc <- c()
sett_proc$dist_colname   <- paste0("pxx_",sett_query$var_dist)
sett_proc$var_dist       <- sett_query$var_dist
sett_proc$var_dist_start <- paste0(sett_proc$var_dist, "_pedal_act_start")
sett_proc$id_colname     <- "passing"
sett_proc$group_colname  <- "pxx_round_txt"



# Summarise pedal activity ------------------------------------------------

dat_pedal_act_summary <- 
  dat_pedal %>% 
  ## Reduce pedal activity to starting arrival measure
  group_by_(sett_proc$id_colname, 
            sett_proc$group_colname, "pedal_act_id") %>% 
  summarise_(.dots = c(
    setNames(list(interp(~ min(var), var = as.name("pedal_act")) ), 
             "pedal_act"),
    setNames(list(interp(~ min(var), var = as.name(sett_proc$dist_colname)) ), 
             sett_proc$var_dist_start)
    )) %>% 
  ## Arrange by id and arrival measure
  arrange_(sett_proc$id_colname, 
           paste0(sett_proc$var_dist, "_pedal_act_start")) %>% 
  ## Enumerate each type of pedal activity
  group_by_(sett_proc$id_colname) %>% 
  mutate(pedal_act_row_nr = row_number()) 


## For lazyeval see:
## https://stackoverflow.com/questions/26003574/r-dplyr-mutate-use-dynamic-variable-names
## https://stackoverflow.com/questions/39252405/using-dplyr-summarise-in-r-with-dynamic-variable



# Visualise pedal activity ------------------------------------------------

plot_pedal_activity <- 
  ggplot() +
  geom_tile(data = dat_pedal,
            aes_string(x = sett_proc$dist_colname,
                       y = sett_proc$id_colname,
                       fill = "factor(pedal_act)",
                       alpha = "pedal_int")) +
  #facet_grid(round_txt ~ pxx, scales = "free", space = "free") +
  facet_grid(pxx_round_txt ~ ., scales = "free", space = "free") +
  scale_fill_manual(values = c("red2", "white", "green2")) + 
  guides(fill = FALSE, alpha = FALSE) + 
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  theme_bw()

#grid.draw(plot_pedal_activity)
ggsave(file.path("plot", "pedal-activity.png"), plot_pedal_activity)



# Create sequence object --------------------------------------------------

dat_pedal_seq <- 
  createSeqObj(
    #dat_pedal %>% filter(round_txt == "t1_50"), 
    dat_pedal, 
    id_colname = sett_proc$id_colname,
    group_colname = sett_proc$group_colname,
    key_colname = sett_proc$dist_colname,
    value_colname = "pedal_act",
    states = c("braking", "no pedal activity", "accelerating"),
    labels = c("b", "n", "a")
  )

dat_pedal <- dat_pedal_seq$dat_ordered



# Visualise ordered pedal sequences ---------------------------------------

plot_pedal_activity_ordered <-
  ggplot() +
  geom_tile(data = dat_pedal,
            aes_string(x = sett_proc$dist_colname,
                       y = sett_proc$id_colname,
                       fill = "factor(pedal_act)",
                       alpha = "pedal_int")) +
  #facet_grid(round_txt ~ pxx, scales = "free", space = "free") + 
  facet_grid(pxx_round_txt ~ ., scales = "free", space = "free") +
  scale_fill_manual(values = c("red2", "white", "green2")) +
  guides(fill = FALSE, alpha = FALSE) + 
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
ggsave(file.path("plot", "pedal-activity_ordered.png"), plot_pedal_activity_ordered)



# Order sequences by braking activity -------------------------------------

## Find first braking activity
dat_braking_dist_first2 <-
  dat_pedal_act_summary %>%
  filter(pedal_act == -1 ) %>% #& after_dist_m12) %>%
  filter(pedal_act_row_nr == min(pedal_act_row_nr))


dat_braking_dist_first <- 
  dat_pedal_act_summary %>% 
  ## Filter distance before arriving
  ## ... to compute correct min. and max. row numbers for pedal activities
  filter_(paste(sett_proc$var_dist_start, "< -10")) %>% 
  group_by_(sett_proc$id_colname, "pedal_act") %>% 
  ## Find minimum and maximum row number 
  ## ... for braking and accelerating activity
  mutate(pedal_act_row_nr_min = min(pedal_act_row_nr),
         pedal_act_row_nr_max = max(pedal_act_row_nr)) %>% 
  ## Code extra column for both activities
  ## Minimum row number for braking
  ## Maximum row number for accelerating
  ## Using zero as workaround for cases with no corresponding pedal activity
  ## (which cannot be filtered using na.rm = T in the following step)
  mutate(pedal_act_row_nr_min_brake = 
           ifelse(pedal_act == -1,
                  pedal_act_row_nr_min,
                  0),
         pedal_act_row_nr_max_acc = 
           ifelse(pedal_act == 1,
                  pedal_act_row_nr_max,
                  0)) %>% 
  group_by_(sett_proc$id_colname) %>% 
  mutate(pedal_act_row_nr_min_brake = 
           max(pedal_act_row_nr_min_brake)) %>% 
  mutate(pedal_act_row_nr_max_acc = 
           max(pedal_act_row_nr_max_acc)) %>% 
  ## Filter for first braking activity
  filter(pedal_act == -1) %>% 
  ## Filter for braking activity after last accelerating activity
  filter(pedal_act_row_nr_min_brake > pedal_act_row_nr_max_acc) %>% 
  ## In case of multiple brakings (without accelerating in-between)
  filter(pedal_act_row_nr == min(pedal_act_row_nr)) %>% 
  data.frame()
  

## Code every pedal activity before this position as zero
dat_pedal_braking <- 
  left_join(dat_pedal,
            dat_braking_dist_first %>% 
              select_(sett_proc$id_colname,
                      sett_proc$var_dist_start))

row_finder <- 
  which(
    dat_pedal_braking[, sett_proc$dist_colname] < 
      dat_pedal_braking[, sett_proc$var_dist_start])

dat_pedal_braking$pedal_act_braking <- -1
dat_pedal_braking$pedal_act_braking[row_finder] <- 0


## Create ordered sequences object
dat_braking_seq <- 
  createSeqObj(
    dat_pedal_braking, 
    id_colname = sett_proc$id_colname,
    group_colname = sett_proc$group_colname,
    key_colname = sett_proc$dist_colname,
    value_colname = "pedal_act_braking",
    states = c("braking", "no pedal activity", "accelerating"),
    labels = c("b", "n", "a")
  )

dat_pedal_braking <- dat_braking_seq$dat_ordered



# Visualise sequences ordered by braking activity -------------------------

plot_pedal_braking_ordered <-
  ggplot() +
  geom_tile(data = dat_pedal_braking,
            aes_string(x = sett_proc$dist_colname,
                       y = sett_proc$id_colname,
                       fill = "as.factor(pedal_act)",
                       alpha = "pedal_int")) +
  #facet_grid(round_txt ~ pxx, scales = "free", space = "free") + 
  facet_grid(pxx_round_txt ~ ., scales = "free", space = "free") +
  scale_fill_manual(values = c("red2", "white", "green2")) +
  #guides(fill = FALSE, alpha = FALSE) + 
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
ggsave(file.path("plot", "pedal-activity_ordered_braking.png"), plot_pedal_braking_ordered)



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
#            aes_string(x = "pxx_dist_m_rnd1",
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


# Order sequences by rolling activity -------------------------------------

## Create filter for filter pedal sequences before arriving reference point
dat_pedal_act_summary <- 
  dat_pedal_act_summary %>% 
  group_by_(before_dist_m2 = 
              paste(sett_proc$var_dist_start, "<= -2" )) %>% 
  mutate(pedal_act_row_nr_within = row_number())


## Find last acceleration activity 
dat_accelerating_dist_last <- 
  dat_pedal_act_summary %>% 
  filter(pedal_act == 0, 
         before_dist_m2) %>% 
  filter(pedal_act_row_nr == max(pedal_act_row_nr)) %>% 
  data.frame()


## Code every pedal activity before this position as one
dat_pedal_rolling <- 
  left_join(dat_pedal,
            dat_accelerating_dist_last %>% 
              select_(sett_proc$id_colname,
                      sett_proc$var_dist_start))

row_finder <- 
  which(
    dat_pedal_rolling[, sett_proc$dist_colname] < 
      dat_pedal_rolling[, sett_proc$var_dist_start])

dat_pedal_rolling$pedal_act_rolling <- dat_pedal_rolling$pedal_act
dat_pedal_rolling$pedal_act_rolling[row_finder] <- 1













# 
# 
# ## Get data
# dat_pedal_rolling <- dat_pedal_braking
# #dat_pedal <- dat_pedal %>% filter(pxx == 3 & round_txt == "t1_50")
# 
# 
# ## Get braking distances (by creating sequence id for each pedal activity)
# dat_rolling_dist <-
#   dat_pedal_rolling %>%
#   group_by_(sett_proc$id_colname) %>%
#   filter_(paste(sett_proc$dist_colname, "<", 
#                 paste0(sett_proc$var_dist, "_braking_start_first"))) %>% 
#   mutate(row_nr = row_number()) %>% 
#   group_by_(sett_proc$id_colname, "pedal_act") %>% 
#   mutate(pedal_act_id = row_nr - row_number()) %>%
#   filter(pedal_act == 0) %>%
#   group_by_(sett_proc$id_colname, "pedal_act_id") %>% 
#   summarise_(.dots = 
#                setNames(list(
#                  interp(~ min(var), 
#                         var = as.name(sett_proc$dist_colname)) ),
#                  paste0(sett_proc$var_dist, "_rolling_start")))
# 
# 
# ## Get starting position of last rolling activity before braking
# dat_rolling_dist_last <- 
#   dat_rolling_dist %>% 
#   group_by_(sett_proc$id_colname) %>% 
#   summarise_(.dots = 
#                setNames(list( 
#                  interp(~ max(var),
#                         var = as.name(paste0(sett_proc$var_dist, "_rolling_start"))) ),
#                  paste0(sett_proc$var_dist, "_rolling_start_last")))
#  
# 
# ## Code every pedal activity before this position as one
# dat_pedal_rolling <- 
#   left_join(dat_pedal_rolling,
#             dat_rolling_dist_last)
# 
# row_finder <- 
#   which(
#     dat_pedal_rolling[, sett_proc$dist_colname] < 
#       dat_pedal_rolling[, paste0(sett_proc$var_dist, "_rolling_start_last")])
# 
# dat_pedal_rolling$pedal_act_rolling <- dat_pedal_rolling$pedal_act
# dat_pedal_rolling$pedal_act_rolling[row_finder] <- 1


## Create ordered sequences object
dat_rolling_seq <- 
  createSeqObj(
    dat_pedal_rolling, 
    id_colname = sett_proc$id_colname,
    group_colname = sett_proc$group_colname,
    key_colname = sett_proc$dist_colname,
    value_colname = "pedal_act_rolling",
    states = c("braking", "no pedal activity", "accelerating"),
    labels = c("b", "n", "a")
  )

dat_pedal_rolling <- dat_rolling_seq$dat_ordered




# Visualise sequences ordered by last rolling activity --------------------

plot_pedal_rolling_ordered <-
  ggplot() +
  geom_tile(data = dat_pedal_rolling,
            aes_string(x = sett_proc$dist_colname,
                       y = sett_proc$id_colname,
                       fill = "as.factor(pedal_act)",
                       alpha = "pedal_int")) +
  #facet_grid(round_txt ~ pxx, scales = "free", space = "free") + 
  facet_grid(pxx_round_txt ~ ., scales = "free", space = "free") +
  scale_fill_manual(values = c("red2", "white", "green2")) +
  #guides(fill = FALSE, alpha = FALSE) + 
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0), breaks = NULL) +
  ggtitle("Pedal activity", 
          subtitle = "Ordered by last rolling before first braking") +
  theme_bw() +
  theme(axis.text.y = element_blank(),
        legend.position = "none",
        panel.background = element_rect(fill = "white"),
        panel.grid = element_blank()) + 


##grid.draw(plot_pedal_braking_ordered)
ggsave(file.path("plot", "pedal-activity_ordered_rolling.png"), 
       plot_pedal_rolling_ordered,
       height = 16, width = 8, units = "cm", dpi = 600)















# Develop -----------------------------------------------------------------

dat_accelerating_dist_last
dat_braking_dist_first


test_acc <- 
  dat_accelerating_dist_last %>% 
  ungroup() %>% 
  group_by(pxx_round_txt) %>% 
  arrange(dist_m_rnd1_pedal_act_start) %>% 
  mutate(row_nr = row_number())

ggplot() + 
  geom_line(data = test_acc,
            aes(x = dist_m_rnd1_pedal_act_start,
                y = row_nr,
                colour = pxx_round_txt)) +
  coord_cartesian(ylim = c(0, 40))

test_brake <- 
  dat_braking_dist_first %>% 
  ungroup() %>% 
  group_by(pxx_round_txt) %>% 
  arrange(dist_m_rnd1_pedal_act_start) %>% 
  mutate(row_nr = row_number())

ggplot() + 
  geom_line(data = test_brake,
            aes(x = dist_m_rnd1_pedal_act_start,
                y = row_nr,
                colour = pxx_round_txt)) +
  coord_cartesian(ylim = c(0, 40))

test_roll_length <- 
  left_join(dat_accelerating_dist_last %>% 
              select(passing, pxx_round_txt, 
                     dist_m_rnd1_pedal_act_start_acc = dist_m_rnd1_pedal_act_start),
            dat_braking_dist_first %>% 
              select(passing, pxx_round_txt, 
                     dist_m_rnd1_pedal_act_start_brake = dist_m_rnd1_pedal_act_start))

test_roll_length$dur <- 
  test_roll_length$dist_m_rnd1_pedal_act_start_acc - 
  test_roll_length$dist_m_rnd1_pedal_act_start_brake

ggplot() + 
  geom_boxplot(data = test_roll_length,
               aes(x = pxx_round_txt,
                   y = dur,
                colour = pxx_round_txt))

# dat_pedal_braking2 <-
#   dat_pedal_braking%>%
#   group_by(passing, pedal_act) %>%
#   mutate(brakepress_bar_cummean = cummean(brakepress_bar),
#          brakepress_bar_dev = sqrt( (brakepress_bar - brakepress_bar_cummean)) )
# 
# 
# ggplot() +
#   geom_line(data = dat_pedal_braking2,
#             aes(x = pxx_dist_m_rnd1,
#                 y = brakepress_bar,
#                 group = passing)) +
#   facet_grid(pxx_round_txt ~ .)
# 
# 
# ggplot() +
#   geom_line(data = dat_pedal_braking2,
#             aes(x = pxx_dist_m_rnd1,
#                 y = brakepress_bar_cummean,
#                 group = passing)) +
#   facet_grid(pxx_round_txt ~ .)
# 
# ggplot() +
#   geom_line(data = dat_pedal_braking2,
#             aes(x = pxx_dist_m_rnd1,
#                 y = brakepress_bar_dev,
#                 group = passing)) +
#   facet_grid(pxx_round_txt ~ .)
# 
# ggplot() +
#   geom_line(data = dat_pedal_braking2,
#             aes(x = pxx_dist_m_rnd1,
#                 y = speed_kmh,
#                 group = passing)) +
#   facet_grid(pxx_round_txt ~ .)
