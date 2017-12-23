computeDistanceToSteerMax <- function(dat,
                                      db_conn_name = "Study-1",
                                      db_src_name = "t_steer_angle_max_summary_wo_outlier",
                                      col_name_position = "pxx",
                                      col_name_dti = "pxx_dti_m_rnd1",
                                      stat_name = "median" ## or mean
) {
  
  ## Get data
  dat_name <- deparseDataFunArg(dat, return_dat = F)
  dat <- deparseDataFunArg(dat)

  ## Get information on DTI where steerangle = max
  dat_sa_summary <- 
    dbGetSrc(dbFindConnObj("Study-1"), "t_steer_angle_max_summary_wo_outlier") 
  
  col_name_dti_stat <- paste_(col_name_dti, "min", stat_name)
  
  dat_sa_summary %>% 
    select_(col_name_position,
            col_name_dti_stat)
  
  if (!stat_name %in% c("median", "mean")) {
    stop("Use 'median' or 'mean' for stat_name")
  }
  
  if (stat_name == "median") {
    
    dat_sa_summary <- 
      dat_sa_summary %>% 
      group_by_(col_name_position) %>% 
      summarize_(.dots = 
                   setNames(list(interp(~ median(v),
                                        v = as.name(col_name_dti_stat))),
                            "correction"))
    
  }
  
  if (stat_name == "mean") {
    
    dat_sa_summary <- 
      dat_sa_summary %>% 
      group_by_(col_name_position) %>% 
      summarize_(.dots = 
                   setNames(list(interp(~ mean(v),
                                        v = as.name(col_name_dti_stat))),
                            "correction"))
    
  }
  
  dat <- 
    left_join(dat,
              dat_sa_summary %>% 
                select_(col_name_position,
                        "correction")) %>% 
    mutate_(.dots = setNames(list(interp(~ a - b,
                                         a = as.name(col_name_dti),
                                         b = as.name("correction") )),
                             paste_(col_name_dti, "steer_max")))

  ## Remove column with correction value
  dat[, "correction"] <- NULL
  
  assign(dat_name, 
         dat, 
         envir = .GlobalEnv)
}


computeDistanceToSteerMax(bckp)

# 
# 
# 
# test_summary <- 
#   bckp %>% 
#   filter(pxx != 3) %>% 
#   group_by(pxx, pxx_dti_m_rnd1) %>% 
#   summarize(steer_angle_deg = mean(steer_angle_deg))
# 
# ggplot() + 
#   geom_line(data = test_summary,
#             aes(x = pxx_dti_m_rnd1,
#                 y = steer_angle_deg,
#                 group = pxx)) 
# 
# 
# test_summary2 <- 
#   bckp %>% 
#   filter(pxx != 3) %>% 
#   group_by(pxx, pxx_dti_m_rnd1_steer_max) %>% 
#   summarize(steer_angle_deg = mean(steer_angle_deg))
# 
# ggplot() + 
#   geom_line(data = test_summary2,
#             aes(x = pxx_dti_m_rnd1_steer_max,
#                 y = steer_angle_deg,
#                 group = pxx)) 
