
# Query settings ----------------------------------------------------------

sett_query <- c()
sett_query$db_conn_name <- "db_conn_7"


# Load GPS data -----------------------------------------------------------

dat_gps <- 
  dbGetQuery(get(sett_query$db_conn_name), 
             "SELECT row_nr, subject_id, time_s, gps_lat, gps_lon FROM t_adtf_formatted")

dat_gps_dist_p02 <- 
  dbGetQuery(get(sett_query$db_conn_name), 
             "SELECT row_nr, p02_gps_dist_m FROM t_adtf_p02_gps_dist")

dat_gps_dist_p03 <- 
  dbGetQuery(get(sett_query$db_conn_name), 
             "SELECT row_nr, p03_gps_dist_m FROM t_adtf_p03_gps_dist")

## dat_gps_dist_p04 not necessary as it refers to same coordinates
## ... as dat_gps_dist_p05

dat_gps_dist_p05 <- 
  dbGetQuery(get(sett_query$db_conn_name), 
             "SELECT row_nr, p05_gps_dist_m FROM t_adtf_p05_gps_dist")

dat_gps_dist_p06 <- 
  dbGetQuery(get(sett_query$db_conn_name), 
             "SELECT row_nr, p06_gps_dist_m FROM t_adtf_p06_gps_dist")

t_rounds_summary <- 
  dbGetQuery(get(sett_query$db_conn_name), 
             "SELECT subject_id, round_id, row_nr_round_start 
             FROM t_adtf_rounds_summary")



# Complete round_id -------------------------------------------------------

## Create template with GPS and round information
dat_gps_rounds <-
  ## Add round information
  left_join(dat_gps,
            t_rounds_summary,
            by = c("subject_id", 
                   "row_nr" = "row_nr_round_start")) %>% 
  group_by(subject_id) %>%
  arrange(row_nr) %>%
  fill(round_id) %>% 
  data.frame()
rm(dat_gps)

dat_gps_p02 <- 
  left_join(dat_gps_rounds,
            dat_gps_dist_p02 %>% select(row_nr, p02_gps_dist_m))

dat_gps_p03 <- 
  left_join(dat_gps_rounds,
            dat_gps_dist_p03 %>% select(row_nr, p03_gps_dist_m))

dat_gps_p05 <- 
  left_join(dat_gps_rounds,
            dat_gps_dist_p05 %>% select(row_nr, p05_gps_dist_m))

dat_gps_p06 <- 
  left_join(dat_gps_rounds,
            dat_gps_dist_p06 %>% select(row_nr, p06_gps_dist_m))



# Find minimum distance to p02 --------------------------------------------

## Explore distances smaller than 10
dat_gps_p02 %>% 
  filter(subject_id == 11) %>% 
  with(plot(time_s, p02_gps_dist_m, ylim = c(0, 10)))

## Find minimum distances
dat_gps_p02_summary <- 
  dat_gps_p02 %>% 
  #filter(subject_id == 1) %>% 
  filter(round_id %in% 0:4) %>% 
  group_by(subject_id, round_id) %>% 
  #mutate(time_s_start_round = min(time_s)) %>% 
  filter(p02_gps_dist_m <= 5) %>% 
  mutate(time_s_diff = time_s - lag(time_s)) %>%
  mutate(time_s_diff = ifelse(is.na(time_s_diff), 0, time_s_diff)) %>%
  mutate(time_cond = ifelse(time_s_diff > 15, 1, 0)) %>% 
  mutate(time_cond = cumsum(time_cond)) %>% 
  group_by(subject_id, round_id, time_cond) %>%
  filter(p02_gps_dist_m == min(p02_gps_dist_m)) %>% 
  ## For testing purposes comment following lines
  group_by(subject_id, round_id) %>% 
  filter(time_s == min(time_s)) %>% 
  data.frame()

## Visualise all minimum distances
dat_gps_p02_summary %>% 
  group_by(subject_id, round_id) %>% 
  with(plot(time_s, p02_gps_dist_m, ylim = c(0, 10)))

## Count all minimum distances
dat_gps_p02_summary %>% 
  group_by(subject_id) %>% 
  summarise(count = n()) %>% 
  data.frame()



# Find minimum distance to p03 --------------------------------------------

## Visualise distances smaller than 10
dat_gps_p03 %>% 
  filter(subject_id == 11) %>% 
  with(plot(time_s, p03_gps_dist_m, ylim = c(0, 10)))

## Necessary for alternative 1: Using timely distance to p02
# dat_gps_p03 <- 
#   left_join(dat_gps_p03,
#             dat_gps_p02 %>% select(row_nr, time_s_p02 = time_s)) %>% 
#   group_by(subject_id, round_id) %>% 
#   mutate(time_s_p02 = min(time_s_p02, na.rm = T)) %>% ## Warnings related to round_id = NA
#   data.frame()

## Find minimum distances
dat_gps_p03_summary <-
  dat_gps_p03 %>% 
  #filter(subject_id == 11) %>% 
  filter(round_id %in% 0:4) %>% 
  group_by(subject_id, round_id) %>%
  filter(p03_gps_dist_m <= 5) %>% 
  mutate(time_s_diff = time_s - lag(time_s)) %>%
  mutate(time_s_diff = ifelse(is.na(time_s_diff), 0, time_s_diff)) %>%
  mutate(time_cond = ifelse(time_s_diff > 15, 1, 0)) %>%
  mutate(time_cond = cumsum(time_cond)) %>%
  group_by(subject_id, round_id, time_cond) %>% 
  ## Avoid confusion with p02
  ## Alternative 1: Using timely distance to p02
  # mutate(time_s_diff_p02 = time_s - time_s_p02) %>%
  # filter(time_s_diff_p02 > 10) %>%
  ## Alternative 2: Find every neares point with timely distance > 15 s
  ## ... and take only last both values if three are found)
  filter(p03_gps_dist_m == min(p03_gps_dist_m)) %>% 
  # in case of two values
  filter(time_s == min(time_s)) %>% 
  group_by(subject_id, round_id) %>% 
  arrange(desc(time_s)) %>% 
  mutate(p03_id = row_number()) %>% 
  arrange(time_s) %>% 
  filter(p03_id %in% 1:2) %>% 
  mutate(p03_id = row_number())
 
## Split for each manoeuvre of interest
dat_gps_p04_summary  <-
  dat_gps_p03_summary %>% 
  filter(p03_id == 2) %>% 
  mutate(p04_gps_dist_m = p03_gps_dist_m) %>% 
  mutate(p03_gps_dist_m = NULL)

dat_gps_p03_summary <-
  dat_gps_p03_summary %>% 
  filter(p03_id == 1)
 
## Visualise all minimum distances
dat_gps_p03_summary %>% 
  group_by(subject_id, round_id) %>% 
  filter(p03_id == 1) %>% 
  with(plot(time_s, p03_gps_dist_m, ylim = c(0, 10)))
dat_gps_p04_summary %>% 
  group_by(subject_id, round_id) %>% 
  filter(p03_id == 2) %>% 
  with(plot(time_s, p04_gps_dist_m, ylim = c(0, 10)))

## Count all minimum distances
dat_gps_p03_summary %>% 
  group_by(subject_id) %>% 
  summarise(count = n()) %>% 
  data.frame()

dat_gps_p04_summary %>% 
  group_by(subject_id) %>% 
  summarise(count = n()) %>% 
  data.frame()


# Find minimum distance to p05 --------------------------------------------

dat_gps_p05_summary <- 
  dat_gps_p05 %>% 
  filter(round_id %in% 0:4) %>% 
  group_by(subject_id, round_id) %>% 
  filter(p05_gps_dist_m == min(p05_gps_dist_m)) %>% 
  # in case of two values
  filter(time_s == min(time_s))



# Find minimum distance to p06 --------------------------------------------

dat_gps_p06_summary <- 
  dat_gps_p06 %>% 
  filter(round_id %in% 0:4) %>% 
  group_by(subject_id, round_id) %>% 
  filter(p06_gps_dist_m == min(p06_gps_dist_m)) %>% 
  # in case of two values
  filter(time_s == min(time_s))


# Visualisation -----------------------------------------------------------

## Select subject
sid = c(1:38, 40)
#sid = 11
rid <- c(0:4)
#rid <- c(-99:99)


## GPS path
ggplot() +
  ## Plot GPS track for first subject
  geom_path(data = dat_gps_rounds %>% 
              filter(subject_id %in% min(sid) & round_id %in% rid),
            aes(x = lon,
                y = lat)) + 
  geom_point(data = dat_gps_rounds %>% 
               filter(subject_id %in% sid & round_id %in% rid & 
                        !is.na(round_id)) %>% 
               group_by(subject_id, round_id) %>% filter(time_s == min(time_s)),
             aes(x = lon,
                 y = lat),
             colour = "red") + 
## Plot left turn maneuver
  geom_vline(data = data.frame(lat = 48.077003, lon = 11.641311),
             aes(xintercept = lon),
             colour = "blue") +
  geom_hline(data = data.frame(lat = 48.077003, lon = 11.641311),
             aes(yintercept = lat),
             colour = "blue") +
  geom_point(data = dat_gps_p02_summary %>% 
               filter(subject_id %in% sid & round_id %in% rid),
             aes(x = lon,
                 y = lat),
             colour = "blue") +
  ## Plot right turn maneuver
  geom_vline(data = data.frame(lat = 48.077008, lon = 11.641461),
             aes(xintercept = lon),
             colour = "green") +
  geom_hline(data = data.frame(lat = 48.077008, lon = 11.641461),
             aes(yintercept = lat),
             colour = "green3") +
  geom_point(data = dat_gps_p03_summary %>% 
               filter(subject_id %in% sid & round_id %in% rid),
             aes(x = lon,
                 y = lat),
             colour = "green3") + 
  ## Plot straight going maneuver
  geom_point(data = dat_gps_p04_summary %>% 
               filter(subject_id %in% sid & round_id %in% rid),
             aes(x = lon,
                 y = lat),
             colour = "purple") +
  ## Plot curve 1
  geom_vline(data = data.frame(lat = 48.075822, lon = 11.641364),
           aes(xintercept = lon),
           colour = "orange") +
  geom_hline(data = data.frame(lat = 48.075822, lon = 11.641364),
             aes(yintercept = lat),
             colour = "orange") +
  geom_point(data = dat_gps_p05_summary %>% 
               filter(subject_id %in% sid & round_id %in% rid),
             aes(x = lon,
                 y = lat),
             colour = "orange") +
  ## Plot curve 2
  geom_vline(data = data.frame(lat = 48.075879, lon = 11.64411),
           aes(xintercept = lon),
           colour = "pink2") +
  geom_hline(data = data.frame(lat = 48.075879, lon = 11.64411),
             aes(yintercept = lat),
             colour = "pink2") +
  geom_point(data = dat_gps_p06_summary %>% 
               filter(subject_id %in% sid & round_id %in% rid),
             aes(x = lon,
                 y = lat),
             colour = "pink2") + 
  ## Adjust plot
  #facet_grid(round_id~.) +
  facet_grid(.~round_id) +
  ggtitle(paste("GPS path of subject_id", sid)) + 
  ## Coordinates for right half of track
  # coord_cartesian(xlim = c(11.64130, 11.64525),
  #                 ylim = c(48.075, 48.0775)) +
  ## Coordinates for intersection
  coord_cartesian(xlim = c(11.64130, 11.6415),
                  ylim = c(48.07695, 48.07705)) +
  theme_bw()




# Descriptive statistics --------------------------------------------------

library(geosphere)

t_gps_positions <- dbGetSrc(sett_query$db_conn_name, "t_gps_reference_positions")

coll <- list()

for (p in c(2, 3, 4, 5, 6)) {
  
  p_txt <- sprintf("p%02d", p)
  
  lat_pxx <- t_gps_positions$gps_lat[t_gps_positions$position_id == p]
  lon_pxx <- t_gps_positions$gps_lon[t_gps_positions$position_id == p]
  dat_gps_pxx_descr <- 
    get(paste0("dat_gps_", p_txt, "_summary")) %>% 
    rowwise() %>% 
    mutate(dist = distm(c(gps_lat, gps_lon), 
                        c(lat_pxx, lon_pxx), 
                        fun = distHaversine),
           dist_lat = distm(c(gps_lat, gps_lon), 
                            c(lat_pxx, lon_pxx), 
                            fun = distHaversine),
           dist_lon = distm(c(gps_lat, gps_lon), 
                            c(lat_pxx, lon_pxx), 
                            fun = distHaversine)) %>% 
    ungroup()
  
  dat_gps_pxx_descr %>% 
    group_by(round_id) %>% 
    summarise(avg = mean(dist),
              stddev = sd(dist),
              avg_lat = mean(dist_lat),
              stddev_lat = sd(dist_lat),
              avg_lon = mean(dist_lon),
              stddev_lon = sd(dist_lon))
  
  coll[[p_txt]] <- dat_gps_pxx_descr
}



# Save data in database ---------------------------------------------------

for(p in c(2, 3, 4, 5, 6)) {
  
  p_txt <- sprintf("p%02d", p)
  
  dbWriteTable(get(sett_query$db_conn_name), 
               paste0("t_adtf_", p_txt, "_gps_dist_min"), 
               get(paste0("dat_gps_", p_txt, "_summary")) %>% 
                 select(row_nr, subject_id, time_s, round_id,
                        gps_lat, gps_lon,
                        paste0(p_txt, "_gps_dist_m")),
               row.names = F,
               overwrite = T)
}

