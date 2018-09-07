
# Settings ----------------------------------------------------------------

## Initiliase settings
sett_synth <- c()

## Data
sett_synth$meta$df_name <- 
  "dat_study2_t_adtf_pxx_full_aggr_dti_rnd1_intrpld_cut"

## Filters
sett_synth$filters$pxx <- 3
sett_synth$filters$condition_speed <- 70

## Column names
sett_synth$col_names$id <- "passing"
sett_synth$col_names$subject_id <- "subject_id"
sett_synth$col_names$pxx <- "pxx"
sett_synth$col_names$condition_speed <- "condition_speed"
sett_synth$col_names$am <- "dti_m_rnd1"
sett_synth$col_names$speed <- "speed_ms"
sett_synth$col_names$speed_u <- 
  paste_(sett_synth$col_names$speed, "u")
sett_synth$col_names$speed_u_smooth <- 
  paste_(sett_synth$col_names$speed_u, "smooth")
sett_synth$col_names$acc_lon <- "acc_lon_ms2"
sett_synth$col_names$acc_lon_max <- "mean"
sett_synth$col_names$acc_lon_max_renamed <- 
  paste_(sett_synth$col_names$acc_lon, "max")

## Database
sett_synth$db$db_name <- "URBAN-MV-VIE_UniBw_Study-2"
sett_synth$db$conn_name <- dbFindConnObj(sett_synth$db$db_name, output = F)
sett_synth$db$src_names$dat_acc_lon_max <- 
  "t_adtf_results_p02_dti_m_rnd1_acc_lon_max_aggr"
## Replace position reference with filter settings for pxx
sett_synth$db$src_names$dat_acc_lon_max <- 
  sub("p\\d{2}", 
      sprintf("p%02d", sett_synth$filters$pxx), 
      sett_synth$db$src_names$dat_acc_lon_max)

## IDM
sett_synth$idm$u_max <- 60 / 3.6
sett_synth$idm$delta <- 4

## Plot
sett_synth$show_plot <- FALSE
sett_synth$plot$col_name_measure <- sett_synth$col_names$speed_u_smooth



# Preprocess data ---------------------------------------------------------

## Get driving data
dat_synth <- 
  get(sett_synth$meta$df_name)

## Filter condition and position
dat_synth <- 
  dat_synth %>% 
  filter_(createFilterString(
    sett_synth$col_names$condition_speed, 
    sett_synth$filters$condition_speed)) %>% 
  filter_(createFilterString(
    sett_synth$col_names$pxx, 
    sett_synth$filters$pxx))

## Get data for max. lon. acc
dat_acc_lon_max <- 
  dbGetSrc(sett_synth$db$conn_name,
           sett_synth$db$src_names$dat_acc_lon_max)

## Filter condition and position
## Select measure for max. lon. acc.
dat_acc_lon_max <- 
  dat_acc_lon_max %>% 
  filter_(paste(sett_synth$col_names$condition_speed, "%in%", 
                sett_synth$filters$condition_speed)) %>% 
  select_(sett_synth$col_names$subject_id, 
          sett_synth$col_names$acc_lon_max) %>% 
  rename_at(sett_synth$col_names$acc_lon_max, 
            funs(sett_synth$col_names$acc_lon_max_renamed))

## Join max. lon. acc.
dat_synth <- 
  left_join(dat_synth, 
            dat_acc_lon_max)

## Select relevant variables
dat_synth <- 
  dat_synth %>% 
  select_(sett_synth$col_names$id,
          sett_synth$col_names$subject_id,
          sett_synth$col_names$am,
          sett_synth$col_names$speed,
          sett_synth$col_names$acc_lon,
          sett_synth$col_names$acc_lon_max_renamed)



# Synthesize speed profiles -----------------------------------------------

## Synthesize speed profiles
dat_synth <- 
  dat_synth %>% 
  group_by_(.dots = 
              c(sett_synth$col_names$id,
                sett_synth$col_names$subject_id)) %>% 
  predLiebner_computeIDM(
    col_name_speed = sett_synth$col_names$speed,
    col_name_acc_lon = sett_synth$col_names$acc_lon,
    col_name_acc_lon_max = sett_synth$col_names$acc_lon_max_renamed,
    delta = sett_synth$idm$delta) 

## Replace missing values with max. desired speed
row_finder <- is.na(dat_synth[, sett_synth$col_names$speed_u])
dat_synth[row_finder, sett_synth$col_names$speed_u] <- sett_synth$idm$u_max

## Set desired speed profiles to max. desired speed after turning
dat_synth <- 
  predLiebner_setMaxIDM(
    dat_synth,
    col_name_id = sett_synth$col_names$id,
    col_name_am = sett_synth$col_names$am,
    col_name_speed = sett_synth$col_names$speed_u,
    u_max = sett_synth$idm$u_max,
    s_max = 10,
    s_max0 = -10)
  
## Smooth synthesized speed profiles
dat_synth <- 
  dat_synth %>% 
  group_by_(.dots = 
              c(sett_synth$col_names$id,
                sett_synth$col_names$subject_id)) %>% 
  mutate_(.dots = setNames(list(
    interp(~ rollAvg(v, k = 30, align = "center"),
           v = as.name(sett_synth$col_names$speed_u))),
    sett_synth$col_names$speed_u_smooth)) %>% 
  data.frame()



# Visualize synthesized profiles ------------------------------------------

if (sett_synth$show_plot) {
  
  plot_synth <- 
    ggplot() + 
    geom_line(data = dat_synth,
              aes_string(x = sett_synth$col_names$am,
                         y = sett_synth$col_names$speed,
                         group = sett_synth$col_names$id)) +
    geom_line(data = dat_synth,
              aes_string(x = sett_synth$col_names$am,
                         y = sett_synth$plot$col_name_measure,
                         group = sett_synth$col_names$id),
              color = "red",
              alpha = 0.5) +
    coord_cartesian(xlim = c(-100, 50),
                    ylim = c(0, 30))
  
  plot(plot_synth)
  
}