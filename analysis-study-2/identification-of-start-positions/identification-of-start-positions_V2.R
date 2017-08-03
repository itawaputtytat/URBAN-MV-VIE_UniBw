
# Load GPS data -----------------------------------------------------------

#dat_gps_pos_p08 <- dbGetQuery(db_conn_9, "SELECT * FROM t_adtf_dist_gps_pos_p08")
dat_dist_gps_pos_p05 <- dbGetQuery(db_conn_9, "SELECT * FROM t_adtf_dist_gps_pos_p05")
dat_dist_gps_pos_p06 <- dbGetQuery(db_conn_9, "SELECT * FROM t_adtf_dist_gps_pos_p06")
dat_dist_gps_pos_p08 <- dbGetQuery(db_conn_9, "SELECT * FROM t_adtf_dist_gps_pos_p08")
dat_dist_gps_pos_p10 <- dbGetQuery(db_conn_9, "SELECT * FROM t_adtf_dist_gps_pos_p10")
t_rounds_identification <- dbGetQuery(db_conn_9, "SELECT subid, round_id, row_nr_round_start FROM t_rounds_identification")



# Complete round_id -------------------------------------------------------

dat_gps <-
  left_join(dat_dist_gps_pos_p05 %>% select(rownr, subid, time_s, gps_lat, gps_lon),
            t_rounds_identification,
            by = c("subid", "rownr" = "row_nr_round_start")) %>% 
#   p06_join(dat_dist_gps_pos_p06 %>% select(rownr, subid, p06_gps_dist_m),
#             by = c("subid", "rownr")) %>% 
#   p06_join(dat_dist_gps_pos_curve1 %>% select(rownr, subid, p08_gps_dist_m),
#             by = c("subid", "rownr")) %>% 
#   p06_join(dat_dist_gps_pos_curve2 %>% select(rownr, subid, p10_gps_dist_m),
#             by = c("subid", "rownr")) %>% 
  group_by(subid) %>%
  arrange(rownr) %>%
  fill(round_id) %>% 
  data.frame()

dat_gps_p08 <- 
  left_join(dat_gps,
            dat_dist_gps_pos_p08 %>% select(rownr, p08_gps_dist_m))

dat_gps_p10 <- 
  left_join(dat_gps,
            dat_dist_gps_pos_p10 %>% select(rownr, p10_gps_dist_m))


# Find two minimum distances for p10 --------------------------------------
#dat_gps_backup <- dat_gps
#dat_gps <- dat_gps_backup
## Einbauen: Links abbiegen muss länger als 15 sekunden her sein nach Rechts-Abbiegen

dat_gps_p05 <- 
  left_join(dat_gps,
            dat_dist_gps_pos_p05 %>% select(rownr, p05_gps_dist_m))

dat_gps_p05_summary <- 
  dat_gps_p05 %>% 
  #filter(subid == 1) %>% 
  filter(round_id %in% 0:4) %>% 
  group_by(subid, round_id) %>% 
  #mutate(time_s_start_round = min(time_s)) %>% 
  filter(p05_gps_dist_m <= 5) %>% 
  mutate(time_s_diff = time_s - lag(time_s)) %>%
  mutate(time_s_diff = ifelse(is.na(time_s_diff), 0, time_s_diff)) %>%
  mutate(time_cond = ifelse(time_s_diff > 15, 1, 0)) %>% 
  mutate(time_cond = cumsum(time_cond)) %>% 
  group_by(subid, round_id, time_cond) %>%
  filter(p05_gps_dist_m == min(p05_gps_dist_m)) %>% 
  ## For testing purposes comment following lines
  group_by(subid, round_id) %>% 
  filter(time_s == min(time_s)) %>% 
  data.frame()

test <- 
  dat_gps_p05_summary %>% 
  group_by(subid, round_id) %>% 
  filter(round_id >= 0 & round_id <= 4)
plot(test$time_s, test$p05_gps_dist_m, ylim = c(0, 10))

dat_gps_p05 %>% 
  group_by(subid) %>% 
  summarise(count = n()) %>% 
  data.frame()

# !!! MIN TIME IST GEFÄHRLICH

dat_gps_p06 <- 
  left_join(dat_gps,
            dat_dist_gps_pos_p06 %>% select(rownr, p06_gps_dist_m))

dat_gps_p06 <- 
  left_join(dat_gps_p06,
            dat_gps_p05 %>% select(rownr, time_s_p05 = time_s)) %>% 
  group_by(subid, round_id) %>% 
  mutate(time_s_p05 = min(time_s_p05, na.rm = T)) %>% ## Warnings related to round_id = NA
  data.frame()

dat_gps_p06_summary <-
  dat_gps_p06 %>% 
  filter(subid == 11) %>% 
  filter(round_id %in% 0:4) %>% 
  group_by(subid, round_id) %>%
  filter(p06_gps_dist_m <= 5) %>% 
  mutate(time_s_diff = time_s - lag(time_s)) %>%
  mutate(time_s_diff = ifelse(is.na(time_s_diff), 0, time_s_diff)) %>%
  mutate(time_cond = ifelse(time_s_diff > 15, 1, 0)) %>%
  mutate(time_cond = cumsum(time_cond)) %>%
  group_by(subid, round_id, time_cond) %>% 
  ## Avoid confusion with p05
  ## Alternative 1: Using timely distance to p05
  # mutate(time_s_diff_p05 = time_s - time_s_p05) %>%
  # filter(time_s_diff_p05 > 10) %>%
  ## Alternative 2: Find every neares point with timely distance > 15 s
  ## ... and take only last both values if three are found)
  filter(p06_gps_dist_m == min(p06_gps_dist_m)) %>% 
  # in case of two values
  filter(time_s == min(time_s)) %>% 
  group_by(subid, round_id) %>% 
  arrange(desc(time_s)) %>% 
  mutate(p06_id = row_number()) %>% 
  arrange(time_s) %>% 
  filter(p06_id %in% 1:2) %>% 
  mutate(p06_id = row_number())
  
test <- 
  dat_gps_p06 %>% 
  filter(subid == 11)
plot(test$time_s, test$p06_gps_dist_m, ylim = c(0, 10))

dat_gps_p06_summary %>% 
  group_by(subid) %>% 
  summarise(count = n()) %>% 
  data.frame()

test <- 
  dat_gps_p06_summary %>% 
  group_by(subid, round_id) %>% 
  filter(round_id >= 0 & round_id <= 4) %>% 
  filter(p06_id == 1) %>% 
  filter(subid == 1)
plot(test$time_s, test$p06_gps_dist_m, ylim = c(0, 10))

test <- 
  dat_gps_p06_summary %>% 
  group_by(subid, round_id) %>% 
  filter(round_id >= 0 & round_id <= 4) %>% 
  filter(p06_id == 2)
plot(test$time_s, test$gps_dist_m_p06, ylim = c(0, 10))




# Join with data ----------------------------------------------------------

# dat_gps_backup <- dat_gps
# 
# dat_gps <- 
#   p06_join(dat_gps,
#             dat_gps_2p06 %>% select(rownr, p06_id))






# Visualisation -----------------------------------------------------------

## Select subject
sid = c(1:38, 40)
sid = 11
rid <- c(0:4)
rid <- c(-99:99)


## GPS path
ggplot() +
  geom_path(data = dat_gps %>% filter(subid %in% min(sid)),
            aes(x = gps_lon,
                y = gps_lat)) + 
  geom_point(data = dat_gps %>% filter(subid %in% sid & round_id %in% rid & !is.na(round_id)) %>% group_by(subid, round_id) %>% filter(time_s == min(time_s)),
             aes(x = gps_lon,
                 y = gps_lat),
             colour = "red") + 
  #############################################################################
  geom_vline(data = data.frame(gps_lat = 48.077003, gps_lon = 11.641311),
             aes(xintercept = gps_lon),
             colour = "blue") +
  geom_hline(data = data.frame(gps_lat = 48.077003, gps_lon = 11.641311),
             aes(yintercept = gps_lat),
             colour = "blue") +
  # geom_point(data = dat_gps %>% filter(subid %in% sid & round_id %in% rid & !is.na(round_id)) %>% group_by(subid, round_id) %>% filter(p05_gps_dist_m == min(p05_gps_dist_m)),
  #            aes(x = gps_lon,
  #                y = gps_lat),
  #            colour = "blue") +
  geom_point(data = dat_gps_p05_summary %>% filter(subid %in% sid & round_id %in% rid & !is.na(round_id)),
             aes(x = gps_lon,
                 y = gps_lat),
             colour = "blue") +
  #############################################################################
  geom_vline(data = data.frame(gps_lat = 48.077008, gps_lon = 11.641461),
             aes(xintercept = gps_lon),
             colour = "green") +
  geom_hline(data = data.frame(gps_lat = 48.077008, gps_lon = 11.641461),
             aes(yintercept = gps_lat),
             colour = "green3") +
  # geom_point(data = dat_gps %>% filter(subid %in% sid & round_id %in% rid & !is.na(round_id)) %>% group_by(subid, round_id) %>% filter(p06_gps_dist_m == min(p06_gps_dist_m)),
  #            aes(x = gps_lon,
  #                y = gps_lat),
  #            colour = "green") +
  geom_point(data = dat_gps_p06_summary %>% filter(subid %in% sid & round_id %in% rid & !is.na(round_id) & p06_id == 1),
             aes(x = gps_lon,
                 y = gps_lat),
             colour = "green3",
             size = 2.5) + 
  #############################################################################
  geom_point(data = dat_gps_p06_summary %>% filter(subid %in% sid & round_id %in% rid & !is.na(round_id) & p06_id == 2),
             aes(x = gps_lon,
                 y = gps_lat),
             colour = "purple") +
  #############################################################################
  geom_vline(data = data.frame(gps_lat = 48.075822, gps_lon = 11.641364),
           aes(xintercept = gps_lon),
           colour = "yellow2") +
  geom_hline(data = data.frame(gps_lat = 48.075822, gps_lon = 11.641364),
             aes(yintercept = gps_lat),
             colour = "yellow2") +
  geom_point(data = dat_gps_p08 %>% filter(subid %in% sid & round_id %in% rid & !is.na(round_id)) %>% group_by(subid, round_id) %>% filter(p08_gps_dist_m == min(p08_gps_dist_m)),
             aes(x = gps_lon,
                 y = gps_lat),
             colour = "yellow2") +
  #############################################################################
  geom_vline(data = data.frame(gps_lat = 48.075879, gps_lon = 11.64411),
           aes(xintercept = gps_lon),
           colour = "pink2") +
  geom_hline(data = data.frame(gps_lat = 48.075879, gps_lon = 11.64411),
             aes(yintercept = gps_lat),
             colour = "pink2") +
  geom_point(data = dat_gps_p10 %>% filter(subid %in% sid & round_id %in% rid & !is.na(round_id)) %>% group_by(subid, round_id) %>% filter(p10_gps_dist_m == min(p10_gps_dist_m)),
             aes(x = gps_lon,
                 y = gps_lat),
             colour = "pink2") + 
  #############################################################################
  facet_grid(round_id~.) +
  ggtitle(paste("GPS path for subid", sid)) + 
  coord_cartesian(xlim = c(11.64130, 11.6415),
#  coord_cartesian(xlim = c(11.64130, 11.64145),
                 ylim = c(48.07695, 48.07705))


coll <- c()
library(geosphere)
invisible(
sapply(unique(dat_gps_p06$subid), function(x) {
  
  for(r in 0:4) {
    for(p in 1:2) {
      rowfinder1 <- which(dat_gps_p06$subid == x & dat_gps_p06$p06_id == p & dat_gps_p06$round_id == r)
      #rowfinder2 <- which(dat_gps_2p06$subid == x & dat_gps_2p06$p06_id == 2 & dat_gps_2p06$round_id == r)
      lat1 <- dat_gps_p06$gps_lat[rowfinder1]
      lon1 <- dat_gps_p06$gps_lon[rowfinder1]
      lat2 <- 48.077008	
      lon2 <- 11.641461
      #lat2 <- dat_gps_2p06$gps_lat[rowfinder2]
      #lon2 <- dat_gps_2p06$gps_lon[rowfinder2]
      dist = distm(c(lat1, lon1), c(lat2, lon2), fun = distHaversine)
      dist_lat = distm(c(lat1, lon2), c(lat2, lon2), fun = distHaversine)
      dist_lon = distm(c(lat2, lon1), c(lat2, lon2), fun = distHaversine)
      coll <<- rbind(coll, 
                     data.frame(subject_id = x, 
                                round_id = r, 
                                part_id = p, 
                                dist = dist, 
                                dist_lat = dist_lat, 
                                dist_lon = dist_lon))
    }
  }
})
)

coll2 <- c()
invisible(
apply(dat_gps_p05, 1, function(x) {
  lat2 <- 48.077003	
  lon2 <- 11.641311
  dist <- distm(c(x["gps_lat"], x["gps_lon"]), c(lat2, lon2), fun = distHaversine)
  dist_lat <- distm(c(x["gps_lat"], lon2), c(lat2, lon2), fun = distHaversine)
  dist_lon <- distm(c(lat2, x["gps_lon"]), c(lat2, lon2), fun = distHaversine)
  coll2 <<- rbind(coll2, data.frame(subject_id = x["subid"],
                                 round_id = x[["round_id"]],
                                 dist = dist,
                                 dist_lat = dist_lat,
                                 dist_lon = dist_lon))
})
)


# dat_gps_p05 %>% 
#   group_by(round_id) %>% 
#   summarise(avg = mean(p05_gps_dist_m),
#             stddev = sd(p05_gps_dist_m))

coll2 %>% 
  group_by(round_id) %>% 
  summarise(avg = mean(dist),
            stddev = sd(dist),
            avg_lat = mean(dist_lat),
            stddev_lat = sd(dist_lat),
            avg_lon = mean(dist_lon),
            stddev_lon = sd(dist_lon))

coll %>% 
  group_by(round_id, part_id) %>% 
  summarise(avg = mean(dist),
            stddev = sd(dist),
            avg_lat = mean(dist_lat),
            stddev_lat = sd(dist_lat),
            avg_lon = mean(dist_lon),
            stddev_lon = sd(dist_lon))

test <- coll %>% filter(part_id == 1)
plot(test$dist)




# Save data in database ---------------------------------------------------

dat_gps_p05 %>% 
  select(subid, round_id, rownr, time_s, p05_gps_dist_m)

dat_gps_p06 %>% 
  filter(p06_id == 1) %>% 
  select(subid, round_id, rownr, time_s, p06_gps_dist_m)
