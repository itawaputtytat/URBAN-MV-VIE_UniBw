
# Settings ----------------------------------------------------------------

sett_proc <- c()
sett_proc$db_name <- "URBAN-MV-VIE_UniBw_Study-2"
sett_proc$db_conn_name <- dbFindConnObj(sett_proc$db_name, output = F)



# Read data ---------------------------------------------------------------

dat <- read_excel(file.choose(), col_names = T, sheet = 1)
# dat <- dat[, c(1, 5:53)]
# colnames(dat) <- c("subject_id", sprintf("vip_%02d", 1:(ncol(dat)-1)))
dat_temp <- dat[, c(1,21:69)]
colnames(dat_temp) <- c("participant_id", 
                        paste_("vip", sprintf("%02d", c(1:49))))
#dat <- sapply(dat, as.numeric)

dbWriteTable(get(sett_proc$db_conn_name), 
             "t_q_screening_vip", 
             dat_temp, 
             overwrite = T, 
             row.names = F)
