
# Preparatory settings ----------------------------------------------------

sett_dat <- c()
sett_dat$dat_name <- "dat_study1_t_adtf_pxx_full_dti_rnd1"
sett_dat$col_name_case <- "passing"
sett_dat$col_name_time <- "time_s"
sett_dat$col_name_prec <- "prec_veh_level" 

sett_file <- c()
sett_file$file_name <- "template_prec_vhcl.xlsx"



# Create timestamps -------------------------------------------------------

dat <- 
  get(sett_dat$dat_name) %>% 
  group_by_(sett_dat$col_name_case) %>% 
  summarise_all(funs(min)) %>% 
  mutate_(.dots = setNames(list(
    interp(~conv.sec2timestamp(v),
           v = as.name(sett_dat$col_name_time) )),
    "timestamp")) %>% 
  mutate_(.dots = setNames(list( 
    interp(~v, 
           v = 999 )), 
    sett_dat$col_name_prec))



# Export as excel file ----------------------------------------------------

library(openxlsx)
write.xlsx(dat, sett_file$file_name, rowNames = F)
