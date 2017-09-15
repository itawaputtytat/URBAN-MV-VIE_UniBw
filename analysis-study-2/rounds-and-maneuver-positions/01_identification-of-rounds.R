
# Query settings ----------------------------------------------------------

sett_query <- c()
sett_query$db_conn_name <- "db_conn_7"



# Load GPS data -----------------------------------------------------------

dat_gps_dist <- 
  dbGetQuery(get(sett_query$db_conn_name), 
             "SELECT * FROM t_adtf_p01_gps_dist")



# Preparatory settings ----------------------------------------------------

## Select subject
sid = 2



# Visual exploration of GPS data ------------------------------------------

## GPS path
ggplot() +
  geom_path(data = dat_gps_dist %>% filter(subject_id == sid),
            aes(x = gps_lon,
                y = gps_lat)) + 
  ggtitle("GPS path")

## Distance to start position
ggplot() +
  geom_line(data = dat_gps_dist %>% filter(subject_id == sid),
            aes(x = time_s,
                y = p01_gps_dist_m)) + 
  ggtitle("Distance to start position")




# Load speed data ---------------------------------------------------------

dat_speed <- 
  dbGetQuery(get(sett_query$db_conn_name), 
             "SELECT row_nr, speed_kmh, acc_lon_ms2 FROM t_adtf_formatted")
dat_gps_dist_speed <- left_join(dat_gps_dist, dat_speed)



# Visual exploration of speed data ----------------------------------------

ggplot() +
  geom_line(data = dat_gps_dist_speed %>% filter(subject_id == sid),
            aes(x = time_s,
                y = speed_kmh)) +
  ggtitle("Speed data")




# Find start of each round ------------------------------------------------

dat_rounds <-
  dat_gps_dist_speed %>% 
  arrange(row_nr) %>% 
  ## Remember previous time value to define start and end time of round
  mutate(time_s_lag = lag(time_s)) %>% 
  ## Filter for rows where current speed value = 0 and next speed vlaue != 0
  ## Version 1 (Deprecated): Using speed == 0
  filter(speed_kmh == 0 & lead(speed_kmh != 0)) %>%
  ## Version 2: Using acceleration
  ## Reason: Acceleration profile will now start at zero
  ## Result will be about 20 more rows
  # filter(speed_kmh == 0 & acclon_ms2 == 0 & lead(acclon_ms2 != 0) |
  #          ## Special case: Subject 26 without long pause between round 3 and 4
  #          ## Has been identified using visual and data exploration (see below)
  #          subject_id == 26 & row_nr == 1032953) %>%
  ## For each subject_id:
  ## Row number for 
  group_by(subject_id) %>% 
  mutate(rnr_diff = ifelse(row_nr - lag(row_nr, default = 0) > 1, 1, 0)) %>% 
  mutate(rnr_diff_sum = cumsum(rnr_diff)) %>% 
  filter(p01_gps_dist_m <= 10) %>% 
  data.frame()



# Find start of each rounds (adapted for subject 26) ----------------------

## Avoid duplicate stoppings
## Filter for stoppings which occur more than 100 s than the previous stopping
dat_rounds <-
  dat_rounds %>%
  arrange(row_nr) %>%
  group_by(subject_id) %>%
  mutate(time_s_diff = lead(time_s) - time_s) %>%
  mutate(time_s_diff = ifelse(is.na(time_s_diff), 100, time_s_diff)) %>%
  filter(time_s_diff >= 100) %>% 
  data.frame()

## Create identifier using row_nr and time
dat_rounds <-
  dat_rounds %>% 
  #group_by(subject_id) %>% 
  mutate(row_nr_round_start = row_nr,
         row_nr_round_end = lead(row_nr - 1, default = max(dat_gps_dist_speed$row_nr))) %>% 
  mutate(time_s_round_start = time_s,
         time_s_round_end = lead(time_s_lag, default = max(dat_gps_dist_speed$time_s))) 

## Adjust times and row numbers for first occurence of first position
## ... to enable filtering for e.g. -15 s
## (otherwise acceleration values would not start at zero)
# dat_rounds_p01 <- 
#   dat_rounds %>% 
#   group_by(subject_id) %>% 
#   filter(rnr_diff_sum == min(rnr_diff_sum)) %>%
#   mutate(time_s_m15 = floor(time_s) - 15) %>% 
#   select(subject_id, row_nr, time_s_m15)
# 
# 
# test <- 
#   left_join(dat_gps_dist_speed, dat_rounds_p01) %>% 
#   filter()



# Count identifitied starts -----------------------------------------------

dat_rounds %>% 
  group_by(subject_id) %>% 
  summarise(count = n()) %>% 
  data.frame()



# Adjust numeration of start positions ------------------------------------

dat_rounds_adjusted <- 
  dat_rounds %>% 
  group_by(subject_id) %>% 
  arrange(subject_id, desc(time_s_round_start)) %>% 
  mutate(round_id = 5 - row_number()) %>% 
  arrange(subject_id, round_id)




# Correct number of problematic cases: 38 & 39 ----------------------------

## Change order of round_id (stepwise for better interpretation)
dat_rounds_adjusted <-
  dat_rounds_adjusted %>% 
  ## Save in temporary numbers
  mutate(round_id = ifelse(subject_id == 38 & round_id == 0, 99, round_id)) %>% 
  mutate(round_id = ifelse(subject_id == 38 & round_id == 4, 41, round_id)) %>% 
  mutate(round_id = ifelse(subject_id == 38 & round_id == 3, 31, round_id)) %>% 
  mutate(round_id = ifelse(subject_id == 38 & round_id == 2, 21, round_id)) %>% 
  ## Code to actual values
  mutate(round_id = ifelse(subject_id == 38 & round_id == -1, 0, round_id)) %>% 
  mutate(round_id = ifelse(subject_id == 38 & round_id ==  1, 2, round_id)) %>% 
  mutate(round_id = ifelse(subject_id == 38 & round_id == 21, 3, round_id)) %>% 
  mutate(round_id = ifelse(subject_id == 38 & round_id == 31, 4, round_id)) %>% 
  mutate(round_id = ifelse(subject_id == 38 & round_id == 41, 1, round_id)) %>% 
  arrange(row_nr_round_start) %>% 
  data.frame()

## GPS paths in 39 visualize

dat_rounds_adjusted <-
  dat_rounds_adjusted %>%
  group_by(subject_id) %>%
  mutate(round_id = ifelse(subject_id == 39 & round_id ==  0, -3, round_id)) %>% 
  mutate(round_id = ifelse(subject_id == 39 & round_id == -1,  0, round_id))



# Find filter for subject 26 ----------------------------------------------

## Reason: No actual pause between round 3 and 4

ggplot() +
  geom_line(data = dat_gps_dist_speed %>% filter(subject_id == 26),
            aes(x = time_s,
                y = speed_kmh)) +
  coord_cartesian(ylim = c(0, 100)) +
  geom_point(data = dat_rounds_adjusted %>% filter(subject_id == 26),
             aes(x = time_s_round_start,
                 y = speed_kmh),
             colour = "red",
             size = 3) +
  geom_point(data = dat_gps_dist_speed %>% filter(subject_id == 26) %>%
               filter(time_s > 985 & time_s < 990),
             aes(x = time_s,
                 y = speed_kmh),
             colour = "green",
             size = 3) +
  geom_text(data = dat_rounds_adjusted %>% filter(subject_id == 26),
            aes(x = time_s_round_start,
                y = 80,
                label = round_id)) +
  ggtitle(26) +
  theme_bw()

## Special case 26
dat_gps_dist_speed %>% 
  filter(subject_id == 26) %>% 
  filter(time_s > 984 & time_s < 990)

## Selection of row number: 1032953
## See Find start of each round (Version 2)



# Visual exploration of starting positions --------------------------------

# #for(i in 38) {
# for(i in 26:40) {
# #for(i in 38:40) {
# #for(i in 1:40) {
#   
#   plotdat <- 
#     ggplot() +
#     geom_line(data = dat_gps_dist_speed %>% filter(subject_id == i),
#               aes(x = time_s,
#                   y = speed_kmh)) + 
#     coord_cartesian(ylim = c(0, 100)) + 
#     geom_point(data = dat_rounds_adjusted %>% filter(subject_id == i),
#                aes(x = time_s_round_start,
#                    y = speed_kmh),
#                colour = "red",
#                size = 3) + 
#     geom_text(data = dat_rounds_adjusted %>% filter(subject_id == i), 
#               aes(x = time_s_round_start,
#                   y = 80,
#                   label = round_id)) + 
#     ggtitle(i) + 
#     theme_bw()
#   
#   plot(plotdat)
#   
#   if (i < 40)
#     pauseAndContinue()
#   
# }



# Save start data in database ---------------------------------------------

dbWriteTable(get(sett_query$db_conn_name), 
             paste0("t_adtf_p01_gps_dist_min"), 
             dat_rounds_adjusted %>% 
               select(row_nr, subject_id, time_s, round_id,
                      gps_lat, gps_lon,
                      p01_gps_dist_m),
             row.names = F,
             overwrite = T)

dbWriteTable(get(sett_query$db_conn_name), 
             "t_adtf_rounds_summary", 
             dat_rounds_adjusted %>%
               select(subject_id, round_id, 
                      row_nr_round_start, row_nr_round_end, 
                      time_s_round_start, time_s_round_end),
             row.names = F,
             overwrite = T)
