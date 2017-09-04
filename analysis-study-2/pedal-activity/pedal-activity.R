## @knitr pedal-activity

# Settings for dynamic processing -----------------------------------------

sett_proc <- c()
sett_proc$df_name <- "study1_t_adtf_pxx_full_dist_m_rnd1_intrpld_cut"
sett_proc$pxx <- 6
sett_proc$colname_arr_meas <- grep("pxx_dist", colnames(get(sett_proc$df_name)), value = T)[1]
sett_proc$colname_arr_meas_start <- paste0(sett_proc$colname_arr_meas, "_start")
sett_proc$colname_arr_meas_end <-   paste0(sett_proc$colname_arr_meas, "_end")
sett_proc$colname_id    <- "passing"
sett_proc$colname_group <- "pxx_round_txt"
sett_proc$crit_arr_means <- 0
sett_proc$crit_arr_means_last_acc_before_braking <- 0

if (sett_proc$pxx == 1) {
  sett_proc$crit_arr_means_last_acc_before_braking <- -15
}

sett_plot <- c()
sett_plot$xlim_min <- sett_query$dist1
sett_plot$xlim_max <- sett_query$dist2
sett_plot$fill <- c("red2", "white", "green2")
sett_plot$file_path <- 
  createFilePath(c("plot", "study1", "pedal-activity"))
sett_plot$file_name_prefix <- 
  paste("study1", sprintf("p%02d", sett_proc$pxx), sep = "_")




# Prepare data ------------------------------------------------------------

## Initialise data
dat_pedal_act <- get(sett_proc$df_name)

## Filter data for situations of interest
#dat_pedal <- filter(dat_pedal, pxx %in% c(2,3)) 
dat_pedal_act <- filter(dat_pedal_act, pxx %in% c(sett_proc$pxx)) 
#dat_pedal <- filter(dat_pedal, pxx_dist_m_rnd1 <= 10)
#dat_pedal <- filter(dat_pedal, round_txt == "t1_50")

if (sett_proc$pxx == 1) {
  dat_pedal_act <- 
    dat_pedal_act %>% 
    filter_(paste(sett_proc$colname_arr_meas, ">= -50"))
}

## Re-code  pxx and round_txt into new variable 
dat_pedal_act$pxx_round_txt <- 
  paste(sprintf("p%02d", dat_pedal_act$pxx), 
        dat_pedal_act$round_txt, sep = "_")

## Correct acceleration pedal position
correctAccPedalPos(dat_pedal_act)

## Code pedal activity
codePedalActivity(dat_pedal_act) 



# Summarise pedal activity ------------------------------------------------

dat_pedal_act_summary <- 
  dat_pedal_act %>% 
  ## Reduce pedal activity to starting arrival measure
  group_by_(sett_proc$colname_id,
            sett_proc$colname_group,
            "pedal_act_id",
            "subject_id") %>%
  summarise_(.dots = c(
    setNames(list(interp(~ min(var), var = as.name("pedal_act")) ), 
             "pedal_act"),
    setNames(list(interp(~ min(var), 
                         var = as.name(sett_proc$colname_arr_meas)) ), 
             sett_proc$colname_arr_meas_start),
    setNames(list(interp(~ max(var), 
                         var = as.name(sett_proc$colname_arr_meas)) ), 
             sett_proc$colname_arr_meas_end)
    )) %>% 
  ## Arrange by id and arrival measure
  arrange_(sett_proc$colname_id, 
           sett_proc$colname_arr_meas_start) %>% 
  ## Enumerate each type of pedal activity
  group_by_(sett_proc$colname_id) %>% 
  mutate(pedal_act_nr = row_number()) 

## For lazyeval see:
## https://stackoverflow.com/questions/26003574/r-dplyr-mutate-use-dynamic-variable-names
## https://stackoverflow.com/questions/39252405/using-dplyr-summarise-in-r-with-dynamic-variable

## Merge data and pedal activity numeration
dat_pedal_act <- 
  left_join(dat_pedal_act,
            dat_pedal_act_summary %>% 
              select_(sett_proc$colname_id, 
                      "pedal_act_id", 
                      "pedal_act_nr"))



# Visualise pedal activity sequence ---------------------------------------

plot_pedalActSeq <- function(dat, 
                             varname_x,
                             varname_y,
                             varname_facet_row,
                             sett_pl = sett_plot,
                             fill_nr_to_grey = NULL) {
  
  if (!is.null(fill_nr_to_grey))
    sett_plot$fill[fill_nr_to_grey] <- "grey" 
  
  plot_dat <- 
    ggplot() +
    geom_tile(data = dat,
              aes_string(x = varname_x,
                         y = varname_y,
                         fill = "factor(pedal_act)",
                         alpha = "pedal_int_perc")) +
    facet_grid(as.formula(paste(varname_facet_row, "~.")), 
               scales = "free", 
               space = "free") +
    scale_fill_manual(values = sett_pl$fill) + 
    guides(fill = FALSE, 
           alpha = FALSE) + 
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_discrete(expand = c(0, 0)) +
    #scale_y_discrete(expand = c(0, 0), breaks = NULL) +
    coord_cartesian(xlim = c(sett_pl$arr_meas_min, 
                             sett_pl$arr_meas_max)) + 
    theme_bw() + 
    theme(panel.grid = element_blank())
  
}

plot_pedal_act <- 
  plot_pedalActSeq(dat_pedal_act, 
                   varname_x = sett_proc$colname_arr_meas,
                   varname_y = sett_proc$colname_id,
                   varname_facet_row = sett_proc$colname_group,
                   sett_plot)

plot_title_txt <- 
  paste(paste0(sett_plot$file_name_prefix, ": "),
        "Pedal activity sequences")

plot_pedal_act <- plot_pedal_act + ggtitle(plot_title_txt)

#grid.draw(plot_pedal_act_seq)
filename <- 
  paste_(sett_plot$file_name_prefix, 
         "pedal-activity_seq.png")
ggsave(file.path(sett_plot$file_path, filename), plot_pedal_act)



# Find last accelerating activity -----------------------------------------

dat_pedal_act_acc_last <- 
  dat_pedal_act_summary %>% 
  group_by_(sett_proc$colname_id) %>% 
  ## Filter for accelerating activity
  filter(pedal_act == 1) %>% 
  ## Filter for distance criteria:
  ## Activity begins after critical distance OR
  ## ... begins before but ends after
  filter_( paste(
    sett_proc$colname_arr_meas_start, ">=", sett_proc$crit_arr_means, "|",
    "(", sett_proc$colname_arr_meas_start, "<=", sett_proc$crit_arr_means, "&",
    sett_proc$colname_arr_meas_end, ">=", sett_proc$crit_arr_means, ")") ) %>% 
  ## In case of multiple accleration activites
  summarise_all(min) %>% 
  ## Complete data for all passings
  right_join( dat_pedal_act_summary %>% 
                distinct_(sett_proc$colname_id,
                          sett_proc$colname_group) ) %>% 
  rename(pedal_act_nr_acc_last = pedal_act_nr) %>% 
  data.frame()
  
  
## Merge data and pedal activity number of last acceleration activity
dat_pedal2 <-
  left_join(dat_pedal_act,
            dat_pedal_act_acc_last %>% 
              select(passing, pedal_act_nr_acc_last),
            by = sett_proc$colname_id)

## Order sequences
dat_pedal_act_acc_last_order <- 
  dat_pedal_act_acc_last %>% 
  arrange_(sett_proc$colname_group, sett_proc$colname_arr_meas_start) %>% 
  ungroup() %>% 
  select_(sett_proc$colname_id) %>% 
  pull() %>% 
  rev()

dat_pedal2[, sett_proc$colname_id] <- 
  factor(dat_pedal2[, sett_proc$colname_id], 
         levels = dat_pedal_act_acc_last_order)



# Visualise acceleration activity after threshold -------------------------

plot_pedal_act_acc_last <- 
  plot_pedalActSeq(dat_pedal2, 
                   varname_x = sett_proc$colname_arr_meas,
                   varname_y = sett_proc$colname_id,
                   varname_facet_row = sett_proc$colname_group,
                   sett_plot,
                   fill_nr_to_grey = 3)

plot_pedal_act_acc_last <- 
  plot_pedal_act_acc_last + 
  geom_tile(data = 
              dat_pedal2 %>% 
              filter(pedal_act_nr == pedal_act_nr_acc_last),
            aes_string(x = sett_proc$colname_arr_meas,
                       y = sett_proc$colname_id),
            #fill = "factor(pedal_act)"),
            fill = "green2")

plot_title_txt <- 
  paste(paste0(sett_plot$file_name_prefix, ": "),
        "Pedal activity sequences with identified acceleration after",
        sett_proc$crit_arr_means)

plot_pedal_act_acc_last <- plot_pedal_act_acc_last + ggtitle(plot_title_txt)

#grid.draw(plot_pedal_act_seq)
filename <- 
  paste_(sett_plot$file_name_prefix, 
         "pedal-activity_seq_acc-after-crit.png")
ggsave(file.path(sett_plot$file_path, filename), plot_pedal_act_acc_last)



# Find last previous accelerating before braking --------------------------

dat_pedal_act_acc_last_before_brake <-
  left_join(dat_pedal_act_summary,
            dat_pedal_act_acc_last %>%
              select_(sett_proc$colname_id,
                      "pedal_act_nr_acc_last"))
# 
# ## Identify accelerating activity after critical distance
rowfinder <-
  which(dat_pedal_act_acc_last_before_brake[, "pedal_act"] == -1 &
          dat_pedal_act_acc_last_before_brake[, sett_proc$colname_arr_meas_start] <= sett_proc$crit_arr_means)
dat_pedal_act_acc_last_before_brake$pedal_act_nr_break_max <- NA
dat_pedal_act_acc_last_before_brake$pedal_act_nr_break_max[rowfinder] <- 
  dat_pedal_act_acc_last_before_brake$pedal_act_nr[rowfinder]


# ## Find pedal activity number of first braking
dat_pedal_act_acc_last_before_brake <-
  dat_pedal_act_acc_last_before_brake %>%
  group_by_(sett_proc$colname_id) %>%
  mutate(pedal_act_nr_break_max = max(pedal_act_nr_break_max, na.rm = T)) %>%
  ## Filter
  filter(pedal_act == 1 & pedal_act_nr < pedal_act_nr_break_max) %>%
  filter_(paste(sett_proc$colname_arr_meas_start, "<=", 
            sett_proc$crit_arr_means_last_acc_before_braking)) %>% 
  #filter(pxx_dist_m_rnd1_pedal_act_end < 0) %>%
  ## In case of multiple accleration activites
  summarise_all(max) %>%
  ## Complete data for all passings
  right_join( dat_pedal_act_summary %>%
                distinct_(sett_proc$colname_id,
                          sett_proc$colname_group) ) %>%
  rename(pedal_act_nr_acc_last_before_brake = pedal_act_nr) %>%
  data.frame()


## Merge data and last previous acclerating activity before braking
dat_pedal3 <-
  left_join(dat_pedal2,
            dat_pedal_act_acc_last_before_brake %>% 
              select_(sett_proc$colname_id, 
                     "pedal_act_nr_acc_last_before_brake"),
            by = sett_proc$colname_id)


## Order sequences
dat_pedal_act_acc_last_before_brake_order <- 
  dat_pedal_act_acc_last_before_brake %>% 
  arrange_(sett_proc$colname_group, sett_proc$colname_arr_meas_end) %>% 
  ungroup() %>% 
  select_(sett_proc$colname_id) %>% 
  pull() %>% 
  rev()

dat_pedal3[, sett_proc$colname_id] <- 
  factor(dat_pedal3[, sett_proc$colname_id], 
         levels = dat_pedal_act_acc_last_before_brake_order)



# Visualise last acceleration activity before braking ---------------------

plot_pedal_act_acc_last_before_brake <- 
  plot_pedalActSeq(dat_pedal3, 
                   varname_x = sett_proc$colname_arr_meas,
                   varname_y = sett_proc$colname_id,
                   varname_facet_row = sett_proc$colname_group,
                   sett_plot,
                   fill_nr_to_grey = 3)

plot_pedal_act_acc_last_before_brake <- 
  plot_pedal_act_acc_last_before_brake + 
  geom_tile(data = 
              dat_pedal3 %>% 
              filter(pedal_act_nr == pedal_act_nr_acc_last_before_brake),
            aes_string(x = sett_proc$colname_arr_meas,
                       y = sett_proc$colname_id),
            #fill = "factor(pedal_act)"),
            fill = "green2")

plot_title_txt <- 
  paste(paste0(sett_plot$file_name_prefix, ": "),
        "Release acceleration pedal")

plot_pedal_act_acc_last_before_brake <- 
  plot_pedal_act_acc_last_before_brake + 
  ggtitle(plot_title_txt)

#grid.draw(plot_pedal_act_seq)
filename <- 
  paste_(sett_plot$file_name_prefix, 
         "pedal-activity_seq_acc-release.png")
ggsave(file.path(sett_plot$file_path, filename), plot_pedal_act_acc_last_before_brake)



# Visualise evolution of releasing acceleration pedal ---------------------

dat_pedal_act_acc_last_before_brake_end_evo <-
  dat_pedal_act_acc_last_before_brake %>%
  select_(sett_proc$colname_id,
          sett_proc$colname_group,
          sett_proc$colname_arr_meas_end) %>% 
  mutate_(.dots = setNames(list(interp(~ replace(var, is.na(var), min(var, na.rm = T)),
                                       var = as.name(sett_proc$colname_arr_meas_end))),
                           sett_proc$colname_arr_meas_end)) %>% 
    # pxx_dist_m_rnd1_pedal_act_end = 
    #        replace(pxx_dist_m_rnd1_pedal_act_end,
    #                is.na(pxx_dist_m_rnd1_pedal_act_end), 
    #                min(pxx_dist_m_rnd1_pedal_act_end, na.rm = T))) %>% 
  group_by_(sett_proc$colname_group) %>%
  arrange_(sett_proc$colname_arr_meas_end) %>%
  mutate(percentage = row_number() / max(row_number()) * 100) %>% 
  group_by_(sett_proc$colname_group, 
            sett_proc$colname_arr_meas_end) %>% 
  summarise(percentage = max(percentage)) %>% 
  data.frame()

  
  
  
  
  
  
  # dat_pedal_act_acc_last_before_brake_end_evo <- 
  #   dat_pedal_act_acc_last_before_brake
  # 
  # row_finder <- 
  #   is.na(dat_pedal_act_acc_last_before_brake_end_evo[, sett_proc$colname_arr_meas_end]) 
  # 
  # dat_pedal_act_acc_last_before_brake_end_evo[rowfinder, ] <- 
  #   min(dat_pedal_act_acc_last_before_brake_end_evo[, sett_proc$colname_arr_meas_end])
  # 
  # dat_pedal_act_acc_last_before_brake_end_evo <-
  #   dat_pedal_act_acc_last_before_brake_end_evo %>%
  #   select_(sett_proc$colname_id,
  #           sett_proc$colname_group,
  #           sett_proc$colname_arr_meas_end) %>% 
  #   # pxx_dist_m_rnd1_pedal_act_end = 
  #   #        replace(pxx_dist_m_rnd1_pedal_act_end,
  #   #                is.na(pxx_dist_m_rnd1_pedal_act_end), 
  #   #                min(pxx_dist_m_rnd1_pedal_act_end, na.rm = T))) %>% 
  #   group_by_(sett_proc$colname_group) %>%
  #   arrange_(sett_proc$colname_arr_meas_end) %>%
  #   mutate(percentage = row_number() / max(row_number()) * 100) %>% 
  #   group_by_(sett_proc$colname_group, 
  #             sett_proc$colname_arr_meas_end) %>% 
  #   summarise(percentage = max(percentage)) %>% 
  #   data.frame()
  # 
  
  
  
  
  

dat_pedal_act_acc_last_before_brake_end_evo <- 
  lapply(unique(dat_pedal_act_acc_last_before_brake_end_evo[, sett_proc$colname_group]), function(x) {
    
    dat <- dat_pedal_act_acc_last_before_brake_end_evo
    dat <- dat[dat[, sett_proc$colname_group] == x, ]
    
    dat <-
      intrpldf(dat %>% data.frame(),
               colname4ref = sett_proc$colname_arr_meas_end,
               min = min(unique(dat_pedal_act[, sett_proc$colname_arr_meas])),
               max = max(unique(dat_pedal_act[, sett_proc$colname_arr_meas])),
               stepsize = 0.1,
               colnames2excl = "percentage",
               replace_preceding = T)
    
    dat$percentage <- na.locf(dat$percentage, na.rm = F)
    ## Replace NA with min percentage
    dat <-
      dat %>%
      group_by_(sett_proc$colname_group) %>%
      mutate(percentage =
               replace(percentage,
                       is.na(percentage),
                       min(percentage, na.rm = T)))
    
    return(dat)
    
  }) %>% bind_rows()

plot_pedal_act_acc_last_before_brake_end_evo <- 
  ggplot() +
  geom_line(data = dat_pedal_act_acc_last_before_brake_end_evo,
            aes_string(x = sett_proc$colname_arr_meas_end,
                       y = "percentage",
                       colour = sett_proc$colname_group)) + 
  scale_x_continuous(expand = c(0, 0)) +
  coord_cartesian(xlim = c(sett_plot$xlim_min, sett_plot$xlim_max),
                  ylim = c(0, 100)) + 
  theme_bw() + 
  ggtitle(paste(paste0(sett_plot$file_name_prefix, ": "),
                "Evolution of releasing acceleration pedal"))


filename <- 
  paste_(sett_plot$file_name_prefix, 
         "pedal-activity_evo_acc-release.png")
ggsave(file.path(sett_plot$file_path, filename), plot_pedal_act_acc_last_before_brake_end_evo)



# Find first braking activity ---------------------------------------------

dat_pedal_act_break_first <- 
  left_join(dat_pedal_act_summary,
            dat_pedal_act_acc_last_before_brake %>% 
              select_(sett_proc$colname_id, 
                     "pedal_act_nr_acc_last_before_brake")) %>% 
  filter(pedal_act == -1) %>% 
  filter(pedal_act_nr > pedal_act_nr_acc_last_before_brake |
           is.na(pedal_act_nr_acc_last_before_brake)) %>% 
  ## In case of multiple braking activites
  summarise_all(min) %>% 
  ## Complete data for all passings
  right_join( dat_pedal_act_summary %>% 
                distinct_(sett_proc$colname_id,
                          sett_proc$colname_group) ) %>% 
  rename(pedal_act_nr_brake_first = pedal_act_nr) %>% 
  data.frame()


## Merge data and last previous acclerating activity before braking
dat_pedal4 <-
  left_join(dat_pedal3,
            dat_pedal_act_break_first %>% 
              select_(sett_proc$colname_id, 
                     "pedal_act_nr_brake_first"),
            by = sett_proc$colname_id)

## Order sequences
dat_pedal_act_break_first_order <- 
  dat_pedal_act_break_first %>% 
  arrange_(sett_proc$colname_group, sett_proc$colname_arr_meas_start) %>% 
  ungroup() %>% 
  select_(sett_proc$colname_id) %>% 
  pull() %>% 
  rev()


dat_pedal4[, sett_proc$colname_id] <- 
  factor(dat_pedal4[, sett_proc$colname_id], 
         levels = dat_pedal_act_break_first_order)



# Visualise first braking activity ----------------------------------------

plot_pedal_act_brake_first <- 
  plot_pedalActSeq(dat_pedal4, 
                   varname_x = sett_proc$colname_arr_meas,
                   varname_y = sett_proc$colname_id,
                   varname_facet_row = sett_proc$colname_group,
                   sett_plot,
                   fill_nr_to_grey = 1)

plot_pedal_act_brake_first <- 
  plot_pedal_act_brake_first + 
  geom_tile(data = 
              dat_pedal4 %>% 
              filter(pedal_act_nr == pedal_act_nr_brake_first),
            aes_string(x = sett_proc$colname_arr_meas,
                       y = sett_proc$colname_id),
            #fill = "factor(pedal_act)"),
            fill = "red2")

plot_title_txt <- 
  paste(paste0(sett_plot$file_name_prefix, ": "),
        "First braking activity")

plot_pedal_act_brake_first <- 
  plot_pedal_act_brake_first + 
  ggtitle(plot_title_txt)


#grid.draw(plot_pedal_act_seq)
filename <- 
  paste_(sett_plot$file_name_prefix, 
         "pedal-activity_seq_brake-after-acc-release.png")
ggsave(file.path(sett_plot$file_path, filename), 
       plot_pedal_act_brake_first)



# Visualise evolution of braking activity ---------------------------------

dat_pedal_act_break_first_evo <-
  dat_pedal_act_break_first %>%
  group_by_(sett_proc$colname_group) %>%
  arrange_("pxx_round_txt", sett_proc$colname_arr_meas_start) %>%
  select_(sett_proc$colname_id, 
          sett_proc$colname_group, 
          sett_proc$colname_arr_meas_start) %>% 
  mutate(percentage = row_number() / max(row_number()) * 100) %>% 
  group_by_(sett_proc$colname_group, 
            sett_proc$colname_arr_meas_start) %>% 
  summarise(percentage = max(percentage)) %>% 
  data.frame()

dat_pedal_act_break_first_evo <- 
  lapply(unique(dat_pedal_act_break_first_evo[, sett_proc$colname_group]), function(x) {
    
    dat <- dat_pedal_act_break_first_evo
    dat <- dat[dat[, sett_proc$colname_group] == x, ]

    dat <-
      intrpldf(dat %>% data.frame(),
               colname4ref = sett_proc$colname_arr_meas_start,
               min = min(unique(dat_pedal_act[, sett_proc$colname_arr_meas])),
               max = max(unique(dat_pedal_act[, sett_proc$colname_arr_meas])),
               stepsize = 0.1,
               colnames2excl = "percentage",
               replace_preceding = T)
    
    dat$percentage <- na.locf(dat$percentage, na.rm = F)
    dat$percentage[is.na(dat$percentage)] <- 0
    
    return(dat)
    
  }) %>% bind_rows()


plot_pedal_act_break_first_evo <- 
  ggplot() +
  geom_line(data = dat_pedal_act_break_first_evo,
            aes_string(x = sett_proc$colname_arr_meas_start,
                       y = "percentage",
                       colour = sett_proc$colname_group)) + 
  scale_x_continuous(expand = c(0, 0)) +
  coord_cartesian(xlim = c(sett_plot$xlim_min, sett_plot$xlim_max),
                  ylim = c(0, 100)) + 
  theme_bw() + 
  ggtitle(paste(paste0(sett_plot$file_name_prefix, ": "),
                "Evolution of first brake press"))


filename <- 
  paste_(sett_plot$file_name_prefix, 
         "pedal-activity_evo_brake-after-acc-release.png")
ggsave(file.path(sett_plot$file_path, filename), 
       plot_pedal_act_break_first_evo)



# Combine evolution of last rolling and braking activity ------------------

plot_pedal_act_evo <- 
  ggplot() +
  geom_line(data = dat_pedal_act_acc_last_before_brake_end_evo,
            aes_string(x = sett_proc$colname_arr_meas_end,
                       y = "percentage",
                       colour = sett_proc$colname_group),
            linetype = "dashed") +
  geom_line(data = dat_pedal_act_break_first_evo,
            aes_string(x = sett_proc$colname_arr_meas_start,
                       y = "percentage",
                       colour = sett_proc$colname_group)) +
  scale_x_continuous(expand = c(0, 0)) +
  coord_cartesian(xlim = c(sett_plot$xlim_min, sett_plot$xlim_max),
                  ylim = c(0, 100)) + 
  theme_bw() + 
  ggtitle(paste(paste0(sett_plot$file_name_prefix, ": "),
                "Evolution of pedal activity"))


filename <- 
  paste_(sett_plot$file_name_prefix, 
         "pedal-activity_evo_acc-release_brake-after-acc-release.png")
ggsave(file.path(sett_plot$file_path, filename), 
       plot_pedal_act_evo)
