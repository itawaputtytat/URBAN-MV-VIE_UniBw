
# Preparatory settings ----------------------------------------------------

sett_dat <- c()
sett_dat$db_conn_name <- dbFindConnObj("Study-1")
sett_dat$col_name_subject <- "subject_id"



# Fetch data --------------------------------------------------------------

sett_dat$src_name <- "t_q_driving_style"
dat_q_ds_by_subject <- 
  dbGetSrc(sett_dat$db_conn_name, sett_dat$src_name) %>% 
  arrange_(sett_dat$col_name_subject) 

sett_dat$src_name <- "t_q_driving_style_by_examiner"
dat_q_ds_by_examiner <- 
  dbGetSrc(sett_dat$db_conn_name, sett_dat$src_name) %>% 
  arrange_(sett_dat$col_name_subject) 

sett_dat$src_name <- "t_q_vip_scores"
dat_q_vip_scores <- 
  dbGetSrc(sett_dat$db_conn_name, sett_dat$src_name) %>% 
  arrange_(sett_dat$col_name_subject) %>% 
  select(subject_id, contains("pr")) %>% 
  select(!subject_id %in% c(19))

sett_dat$src_name <- "t_q_driving_experience"
dat_q_driving_experience <- 
  dbGetSrc(sett_dat$db_conn_name, sett_dat$src_name) %>% 
  arrange_(sett_dat$col_name_subject) %>% 
  select(subject_id, driving_licence_years, km_overall,
         roadtype_percentage_city,
         roadtype_percentage_town)

dat_q_test <- 
  left_join(dat_q_ds_by_subject,
            dat_q_ds_by_examiner) %>% 
  left_join(dat_q_vip_scores) %>% 
  left_join(dat_q_driving_experience)



# Analysis ----------------------------------------------------------------

result <- corr.test(dat_q_test[-1], adjust = "none")
print(corr.p(result$r, n = length(dat_q_test$subject_id)), digits = 3, short = F)
