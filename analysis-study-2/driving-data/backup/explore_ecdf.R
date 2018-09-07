
# Settings ----------------------------------------------------------------

## Data
sett <- c()
sett$df_name <- "dat_study2_t_adtf_pxx_full_aggr_dti_rnd1_intrpld_cut"
sett$col_names$pxx <- "pxx"
sett$col_names$dti <- "dti_m_rnd1"
sett$col_names$measure <- "acc_lon_ms2"
sett$col_names$group <- "passing"
sett$filters$pxx <- 1
sett$filters$dti_

## Processing
sett$proc$statistic <- "mean"
sett$col_names$measure_statistic <- 
  paste_(sett$col_names$measure,
         sett$proc$statistic)



# Preprocess data ---------------------------------------------------------

## Filter data
dat <- 
  get(sett$df_name) %>% 
  filter_(paste(sett$col_names$pxx, "==", sett$filters$pxx))



# viz ---------------------------------------------------------------------

plot_raw <- 
  ggplot() +
  geom_line(data = dat,
            aes_string(x = sett$col_names$dti,
                       y = sett$col_names$measure,
                       group = sett$col_names$group)) +
  facet_grid(pxx~round_id)

plot(plot_raw)



# Limit data range --------------------------------------------------------

## Filter data
dat_filtered <- 
  dat %>% 
  ## Filter for distance
  filter_(paste(sett$col_names$dti, "<= 25")) %>% 
  ## Filter for positive values only
  filter_(paste(sett$col_names$measure, ">= 0"))



# Extract statistics ------------------------------------------------------

## Compute statistics
dat_filtered_summary <- 
  computeSummary(dat_filtered, 
                 col_names_group = c(sett$col_names$group, "round_id"),
                 col_names_values = sett$col_names$measure)

## Compute ECDF
dat_filtered_summary <- 
  dat_filtered_summary %>% 
  group_by(round_id) %>% 
  arrange_(sett$proc$statistic) %>% 
  mutate_(.dots =
            setNames(list(
              interp(~ computeECDF(var)$y,
                     var = as.name(sett$proc$statistic))),
              "ecdf_prob"))



# Visualize statistics ----------------------------------------------------

plot_ecdf <- 
  ggplot() + 
  geom_step(data = dat_filtered_summary,
            aes_string(x = sett$proc$statistic ,
                       y = "ecdf_prob",
                       group = "round_id",
                       color = "round_id"))

plot(plot_ecdf)

plot_boxplot <- 
  ggplot() + 
  geom_violin(data = dat_filtered_summary,
              aes_string(x = "round_id",
                         y = sett$proc$statistic)) + 
  geom_boxplot(data = dat_filtered_summary,
               aes_string(x = "round_id",
                          y = sett$proc$statistic))

plot(plot_boxplot)              