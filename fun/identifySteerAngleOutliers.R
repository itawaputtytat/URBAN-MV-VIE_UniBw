
identifySteerAngleOutliers <- function(dat, 
                                       col_name_am,
                                       col_name_case,
                                       col_name_group,
                                       col_name_sa,
                                       am_thresholds_sa_max = c(-20, 25),
                                       am_thresholds_sa_min1 = c(-50, 50),
                                       am_thresholds_sa_min2 = c(-50, 50),
                                       am_thresholds_adapt = c(0, 0, 0),
                                       sa_threshold_min = 100,
                                       z_cut_off = c(1.96, 1.96, 1.96),
                                       outlier_sum_threshold = 3,
                                       return_cases_only = F) {
  
  outputFunProc(R)
  
  ## Get data
  dat_name <- deparseDataFunArg(dat, return_dat = F)
  dat <- deparseDataFunArg(dat)
  
  
  # Identify AM of max. SA ------------------------------------------------
  
  ## Find position where steer_angle_deg == max(abs(steer_angle_deg))
  dat_max <- 
    dat %>%
    filter_(paste(col_name_am, ">=", 
                  am_thresholds_adapt[1] + am_thresholds_sa_max[1], "&",
                  col_name_am, "<=", 
                  am_thresholds_adapt[1] + am_thresholds_sa_max[2])) %>% 
    
    group_by_(.dots = c(col_name_group, 
                        col_name_case)) %>%
    filter_( paste("abs(", col_name_sa, ")", "==", 
                   "max(abs(", col_name_sa, "))") ) %>% 
    
    ## Summarize values including AM values
    ## (in case of duplicates with same value take the minimum position)
    ## ... and also the corresponding speed
    summarise_(.dots = c(
      setNames(list(interp(~ min(v), v = as.name(col_name_am))),
               paste_(col_name_am, "min")),
      setNames(list(interp(~ max(v), v = as.name(col_name_sa))),
               paste_(col_name_sa, "max"))
    )) %>% 
    arrange_(paste_(col_name_am, "min"))
  
  
  
  # Summarize data corresponding to max. SA -------------------------------
  
  dat_max_summary <-
    computeSummary(dat_max,
                   col_name_group,
                   c(paste_(col_name_am, "min"),
                     paste_(col_name_sa, "max")),
                   c("min", "max", "mean", "sd", "median"))
  
  
  
  # Extract AM of max. SA -------------------------------------------------
  
  dat_am_steer_max <- 
    dat_max_summary[[paste_(col_name_am, "min")]] %>% 
    select_("median") %>%
    pull() %>% 
    as.vector() %>% 
    median()
  
  ## Adaptive thresholds can be set to fixed value
  ## ... or depending on dat_am_steer_max (requires value T)
  if (am_thresholds_adapt[2]) {
    am_thresholds_adapt[2] <- dat_am_steer_max
  }
  
  if (am_thresholds_adapt[3]) {
    am_thresholds_adapt[3] <- dat_am_steer_max
  }
  
  
  
  # Identify AM of min. SA before turning ---------------------------------
  
  dat_min1 <- 
    left_join(dat,
              dat_max %>%
                select("passing",
                       paste_(col_name_am, "min"))) %>%
    rename_at(paste_(col_name_am, "min"), funs(paste0("sa_max_dti"))) %>% 
    
    filter_(paste(col_name_am, ">=", 
                  am_thresholds_adapt[2] + am_thresholds_sa_min1[1], "&",
                  col_name_am, "<=", 
                  am_thresholds_adapt[2] + am_thresholds_sa_min1[2])) %>% 
    filter_(paste("abs(", col_name_sa, ")", "<=", sa_threshold_min)) %>% 
    filter_(paste(col_name_am, "<=", dat_am_steer_max)) %>% 
    
    group_by_(col_name_group, 
              col_name_case) %>%
    summarise_(.dots = c(
      setNames(list(interp(~ max(v), v = as.name(col_name_am))),
               paste_(col_name_am, "max")),
      setNames(list(interp(~ max(v), v = as.name(col_name_sa))),
               paste_(col_name_sa, "min"))
    )) %>% 
    arrange_(paste_(col_name_am, "max"))
  
  
  
  # Summarize data corresponding to min. SA before turning ------------------
  
  dat_min1_summary <-
    computeSummary(dat_min1,
                   col_name_group,
                   c(paste_(col_name_am, "max"),
                     paste_(col_name_sa, "min")),
                   c("min", "max", "mean", "sd", "median"))

  
  
  # Extract AM of min. SA before turning ------------------------------------
  
  dat_am_steer_min1 <- 
    dat_min1_summary[[paste_(col_name_am, "max")]] %>% 
    select_("median") %>%
    pull() %>% 
    as.vector() %>% 
    median()
  
  
  
  # Identify AM of min. SA after turning ------------------------------------
  
  dat_min2 <- 
    dat %>%
    filter_(paste(col_name_am, ">=", 
                  am_thresholds_adapt[3] + am_thresholds_sa_min2[1], "&",
                  col_name_am, "<=", 
                  am_thresholds_adapt[3] + am_thresholds_sa_min2[2])) %>% 
    filter_(paste(col_name_am, ">=", dat_am_steer_max)) %>% 
    filter_(paste("abs(", col_name_sa, ")", "<=", sa_threshold_min)) %>% 
    group_by_(col_name_group, 
              col_name_case) %>%
    # filter_( paste("abs(", col_name_sa, ")", "==", 
    #                "min(abs(", col_name_sa, "))") ) %>% 
    ## Summarize values including AM values
    ## (in case of duplicates with same value take the minimum position)
    ## ... and also the corresponding speed
    summarise_(.dots = c(
      setNames(list(interp(~ min(v), v = as.name(col_name_am))),
               paste_(col_name_am, "min")),
      setNames(list(interp(~ max(v), v = as.name(col_name_sa))),
               paste_(col_name_sa, "min"))
    )) %>% 
    arrange_(paste_(col_name_am, "min"))
  
  
  
  # Summarize data corresponding to min. steering angle after turning -------
  
  dat_min2_summary <-
    computeSummary(dat_min2,
                   col_name_group,
                   c(paste_(col_name_am, "min"),
                     paste_(col_name_sa, "min")),
                   c("min", "max", "mean", "sd", "median"))
  
  
  
  # Extract AM of min. SA after turning -------------------------------------
  
  dat_am_steer_min2 <- 
    dat_min2_summary[[paste_(col_name_am, "min")]] %>% 
    select_("median") %>%
    pull() %>% 
    as.vector() %>% 
    median()
  
  
  
  # Identify outliers -------------------------------------------------------
  
  dat_max_outlier <- 
    dat_max %>% 
    mutate_(.dots = 
              setNames(list(interp(~ v - dat_am_steer_max,
                                   v = as.name(paste_(col_name_am, "min")))),
                       "diff")) %>% 
    mutate(is_outlier = codeOutliersZ(diff, zCutOff = z_cut_off[1])) %>% 
    select_(col_name_case, 
            .dots = setNames(list(interp(~ is_outlier)), "outlier_steer_max"))
  
  dat_min1_outlier <- 
    dat_min1 %>% 
    mutate_(.dots = 
              setNames(list(interp(~ v - dat_am_steer_min1,
                                   v = as.name(paste_(col_name_am, "max")))),
                       "diff")) %>% 
    mutate(is_outlier = codeOutliersZ(diff, zCutOff = z_cut_off[2])) %>% 
    select_(col_name_case, 
            .dots = setNames(list(interp(~ is_outlier)), "outlier_steer_min1"))
  
  dat_min2_outlier <- 
    dat_min2 %>% 
    mutate_(.dots =
              setNames(list(interp(~ v - dat_am_steer_min2,
                                   v = as.name(paste_(col_name_am, "min")))),
                       "diff")) %>% 
    mutate(is_outlier = codeOutliersZ(diff, zCutOff = z_cut_off[3])) %>% 
    select_(col_name_case, 
            .dots = setNames(list(interp(~ is_outlier)), "outlier_steer_min2"))
  
  dat_outlier <- 
    left_join(dat_max_outlier,
              left_join(dat_min1_outlier,
                        dat_min2_outlier)) %>% 
    group_by_(col_name_case) %>% 
    mutate(outlier_sum = sum(outlier_steer_max,
                             outlier_steer_min1,
                             outlier_steer_min2)) %>% 
    mutate(is_outlier = ifelse(outlier_sum == outlier_sum_threshold, T, F)) %>% 
    arrange(outlier_sum)

  

  # Create final outlier data ---------------------------------------------
  
  # Combine data on outliers and AM on max. SA
  # Add info on correction direction 
  dat_outlier <- 
    left_join(dat_outlier,
              dat_max) %>% 
    mutate_(.dots = setNames(list(
      interp(~ pos_max - pos, 
             pos = as.name(paste_(col_name_am, "min")),
             pos_max = dat_am_steer_max)),
      paste_(col_name_am, "min", "vs_median"))) %>% 
    # mutate_(.dots = setNames(list(
    #   interp(~ f(pos > pos_max, "backward", "forward"), 
    #          f = as.name("ifelse"), 
    #          pos = as.name(paste_(col_name_am, "min")), 
    #          pos_max = dat_am_steer_max)), "correction_direction")) %>% 
    data.frame()
  
  

    # Return list of data frames --------------------------------------------
  
  if (!return_cases_only) {
    return(list(#dat_max = dat_max,
      dat_max_summary = dat_max_summary,
      #dat_am_steer_max = dat_am_steer_max,
      #dat_min1 = dat_min1,
      #dat_min1_summary = dat_min1_summary,
      #dat_am_steer_min1 = dat_am_steer_min1,
      #dat_min2 = dat_min2,
      #dat_min2_summary = dat_min2_summary,
      #dat_am_steer_min2 = dat_am_steer_min2,
      dat_outlier = dat_outlier))
  } else {
    dat_outlier[dat_outlier$is_outlier, col_name_case]
  }

}
