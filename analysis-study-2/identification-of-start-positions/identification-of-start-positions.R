
# Load GPS data -----------------------------------------------------------

dat_gps <- dbGetQuery(db_conn_9, "SELECT * FROM t_adtf_dist_gps_pos_p01")



# Preparatory settings ----------------------------------------------------

## Select subject
sid = 2



# Visual exploration of GPS data ------------------------------------------

## GPS path
ggplot() +
  geom_path(data = dat_gps %>% filter(subid == sid),
            aes(x = gps_lon,
                y = gps_lat)) + 
  ggtitle("GPS path")

## Distance to start position
ggplot() +
  geom_line(data = dat_gps %>% filter(subid == sid),
            aes(x = time_s,
                y = p01_gps_dist_m)) + 
  ggtitle("Distance to start position")




# Load speed data ---------------------------------------------------------

dat_speed <- dbGetQuery(db_conn_9, "SELECT rownr, speed_kmh FROM t_can_format")

dat_gps_speed <- left_join(dat_gps, dat_speed)



# Visual exploration of speed data ----------------------------------------

ggplot() +
  geom_line(data = dat_gps_speed %>% filter(subid == sid),
            aes(x = time_s,
                y = speed_kmh)) +
  ggtitle("Speed data")




# Find start of each round ------------------------------------------------

dat_rounds <-
  dat_gps_speed %>% 
  ## Remember previous time value to define start and end time of round
  mutate(time_s_lag = lag(time_s)) %>% 
  ## Filter for rows where current speed value = 0 and next speed vlaue != 0
  filter(speed_kmh == 0 & lead(speed_kmh != 0)) %>% 
  ## For each subid:
  ## Row number for 
  group_by(subid) %>% 
  mutate(rnr_diff = ifelse(rownr - lag(rownr, default = 0) > 1, 1, 0)) %>% 
  mutate(rnr_diff_sum = cumsum(rnr_diff)) %>% 
  filter(p01_gps_dist_m <= 10)

## Avoid duplicate stoppings
## Filter for stoppings which occur more than 100 s than the previous stopping
dat_rounds <-
  dat_rounds %>%
  arrange(rownr) %>%
  group_by(subid) %>%
  mutate(time_s_diff = lead(time_s) - time_s) %>%
  mutate(time_s_diff = ifelse(is.na(time_s_diff), 100, time_s_diff)) %>%
  filter(time_s_diff >= 100)

## Create identifier using rownr and time
dat_rounds <-
  dat_rounds %>% 
  group_by(subid) %>% 
  mutate(row_nr_round_start = rownr,
         row_nr_round_end = lead(rownr - 1, default = 999999)) %>% 
  mutate(time_s_round_start = time_s,
         time_s_round_end = lead(time_s_lag, default = 999999)) 



# Count identifitied starts -----------------------------------------------

dat_rounds %>% 
  group_by(subid) %>% 
  summarise(count = n()) %>% 
  data.frame()



# Adjust numeration of start positions ------------------------------------

dat_rounds_adjusted <- 
  dat_rounds %>% 
  group_by(subid) %>% 
  arrange(subid, desc(time_s_round_start)) %>% 
  mutate(round_id = 5 - row_number()) %>% 
  arrange(subid, round_id)




# Correct number of problematic cases: 38 & 39 ----------------------------

## Change order of round_id (stepwise for better interpretation)
dat_rounds_adjusted <-
  dat_rounds_adjusted %>% 
  ## Save in temporary numbers
  mutate(round_id = ifelse(subid == 38 & round_id == 0, 99, round_id)) %>% 
  mutate(round_id = ifelse(subid == 38 & round_id == 4, 41, round_id)) %>% 
  mutate(round_id = ifelse(subid == 38 & round_id == 3, 31, round_id)) %>% 
  mutate(round_id = ifelse(subid == 38 & round_id == 2, 21, round_id)) %>% 
  ## Code to actual values
  mutate(round_id = ifelse(subid == 38 & round_id == -1, 0, round_id)) %>% 
  mutate(round_id = ifelse(subid == 38 & round_id == 1, 2, round_id)) %>% 
  mutate(round_id = ifelse(subid == 38 & round_id == 21, 3, round_id)) %>% 
  mutate(round_id = ifelse(subid == 38 & round_id == 31, 4, round_id)) %>% 
  mutate(round_id = ifelse(subid == 38 & round_id == 41, 1, round_id)) %>% 
  arrange(row_nr_round_start) %>% 
  data.frame()

## GPS paths in 39 visualize

dat_rounds_adjusted <-
  dat_rounds_adjusted %>%
  group_by(subid) %>%
  mutate(round_id = ifelse(subid == 39 & round_id == 0, -3, round_id)) %>% 
  mutate(round_id = ifelse(subid == 39 & round_id == -1, 0, round_id))



# Visual exploration of starting positions --------------------------------

for(i in 38) {
#for(i in 38:40) {
#for(i in 1:40) {
  
  plotdat <- ggplot() +
    geom_line(data = dat_gps_speed %>% filter(subid == i),
              aes(x = time_s,
                  y = speed_kmh)) + 
    coord_cartesian(ylim = c(0, 100)) + 
    geom_point(data = dat_rounds_adjusted %>% filter(subid == i),
               aes(x = time_s_round_start,
                   y = speed_kmh),
               colour = "red",
               size = 3) + 
    geom_text(data = dat_rounds_adjusted %>% filter(subid == i), 
              aes(x = time_s_round_start,
                  y = 80,
                  label = round_id)) + 
    ggtitle(i)
  
  plot(plotdat)
  
  if (i < 40)
    pauseAndContinue()
  
}



# Save start data in database ---------------------------------------------

dat_rounds_adjusted <- 
  dat_rounds_adjusted %>% 
  select(subid, round_id, 
         row_nr_round_start, row_nr_round_end, 
         time_s_round_start, time_s_round_end)

dbWriteTable(db_conn_9, 
             "t_rounds_identification", 
             dat_rounds_adjusted,
             row.names = F,
             overwrite = T)
