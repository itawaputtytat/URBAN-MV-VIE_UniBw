sett_query <- c()
sett_query$db_name <- "Blaschke"
sett_query$db_conn_name <- dbFindConnObj(sett_query$db_name, output = F)

dat_tables <- dbListTables(get(sett_query$db_conn_name))
dat_tables <- grep("kreuzung", dat_tables, value = T)

dat <- c()
for(t in dat_tables) {
  dat_temp <- dbGetSrc(sett_query$db_conn_name, t)
  dat_temp$db_src_name <- t
  dat <- rbind(dat, dat_temp)
}

dat <- 
  dat %>% 
  group_by(db_src_name) %>% 
  mutate(Lenkradwinkel_max = max(Lenkradwinkel)) %>% 
  mutate(time_at_Lenkradwinkel_max = ifelse(Lenkradwinkel == Lenkradwinkel_max,
                                            Time,
                                            NA)) %>% 
  mutate(time_at_Lenkradwinkel_max = max(time_at_Lenkradwinkel_max, na.rm = T)) %>% 
  mutate(time_v2 = Time - min(Time)) %>% 
  mutate(time_v3 = Time - time_at_Lenkradwinkel_max)

ggplot() + 
  geom_line(data = dat,
            aes(x = time_v3,
                y = Geschwindigkeit_15Bit,
                group = db_src_name))