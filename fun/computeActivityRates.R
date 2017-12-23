
# Objective ---------------------------------------------------------------
## Compute glance rates for each direction in two versions
## 1) As percentage of first glances
## 2) As percentage of overall glances

computeActivityRates <- function(dat, 
                                 col_name_act_level, 
                                 col_name_am,
                                 col_name_case_id,
                                 col_name_time = "time_s",
                                 unique_level) {
  
  outputFunProc(R)
  
  ## Replace missing values with 0
  dat[is.na(dat)] <- 0
  
  ## Detect number of cases with available glance data
  cases_n <- 
    dat %>% 
    #filter_(paste("!is.na(", col_name_act_level, ")")) %>% 
    distinct_(col_name_case_id) %>% 
    summarise(n = n()) %>% 
    pull(n)
  
  
  # Create template for data completion -------------------------------------
  
  template <- 
    dat %>% 
    select_(col_name_am) %>% 
    distinct() %>% 
    arrange_(col_name_am)
  
  
  
  # Create activity ids -----------------------------------------------------
  
  dat <-
    dat %>%
    select_(col_name_am,
            col_name_time,
            col_name_case_id,
            col_name_act_level) %>% 
    
    ## Order by case and time
    ## Create row numbers over complete data frame
    arrange_(col_name_case_id, col_name_time) %>%
    mutate(row_nr = row_number()) %>% 
    
    ## Create row numbers per case and activity level
    ## Create id for current glance sequence
    group_by_(col_name_case_id, col_name_act_level) %>%
    mutate(act_row_nr = row_number()) %>%
    
    ## Create activity id 
    mutate(act_id = (row_nr - act_row_nr)) %>% 
    mutate(row_nr = NULL,
           act_row_nr = NULL)
  
  act_n <- length(unique(dat$act_id))
  
  ## In case of uncertainty call following line for proofing uniqueness of ids
  ## View(dat %>%
  ## group_by(subject_id, act_id)  %>%
  ## summarise(n_unique_act_ids = length(unique(act_id))))
  
  
  
  # Computations related to longitudinal reference --------------------------
  
  dat_am <- 
    dat %>%
    
    ## Minimum AM for each activity
    group_by(act_id) %>% 
    mutate_(.dots = setNames(list(interp(~ min(v), v = as.name(col_name_am))),
                             "am_min_PER_act_id")) %>% 
    
    ## Minimum AM for each activity per case and activity level
    group_by_(col_name_case_id, col_name_act_level) %>%
    mutate(am_min_PER_case_id_and_act_level = min(am_min_PER_act_id)) %>%
    
    ## Minimum AM for each activity level
    group_by_(col_name_act_level) %>%
    mutate(am_min_PER_act_level = min(am_min_PER_act_id)) %>%
    
    ## Summarise by case and activity (only one row per activity id)
    group_by_(col_name_case_id, "act_id", col_name_act_level) %>%
    summarise(am_min_PER_act_id = min(am_min_PER_act_id),
              am_min_PER_case_id_and_act_level = min(am_min_PER_case_id_and_act_level),
              am_min_PER_act_level = min(am_min_PER_act_level)) %>% 
    arrange_(col_name_act_level, "am_min_PER_act_id") 
  
  
  
  # Enumerate and count all activities --------------------------------------
  
  dat_n <-
    dat_am %>% 
    
    ## Enumerate and count activites per case
    group_by_(col_name_case_id) %>% 
    arrange_(col_name_case_id, "am_min_PER_act_id") %>% 
    mutate(act_nr_PER_case_id = row_number(),
           act_n_PER_case_id = length(unique(act_id))) %>%
    
    ## Enumeriate and count activities per case and activity level
    group_by_(col_name_case_id, col_name_act_level) %>%
    mutate(act_nr_PER_case_id_and_act_level = row_number(),
           act_n_PER_case_id_and_act_level = length(unique(act_id))) %>%
    
    ## Enumerate and count activites per activity level
    group_by_(col_name_act_level) %>%
    arrange_(col_name_act_level, "am_min_PER_act_id") %>% 
    mutate(act_nr_PER_act_level = row_number(),
           act_n_PER_act_level = length(unique(act_id)))
  
  
  
  # Enumerate and count first activities ------------------------------------
  
  dat_n_1st <- 
    dat_n %>% 
    
    ## Filter for first activity per case and activity level
    filter(act_nr_PER_case_id_and_act_level == 1) %>% 
    
    ## Enumerate and count first activities by activity level
    group_by_(col_name_act_level) %>% 
    arrange_(col_name_act_level, "am_min_PER_act_id") %>% 
    mutate(act_nr_PER_act_level_1st = row_number(),
           act_n_PER_act_level_1st = n())
  
  
  
  # Compute ratio for all activities ----------------------------------------
  
  dat_n_ratio <- 
    dat_n %>% mutate(
      
      ## Rate of activity number per case 
      ## ... on total activities per case
      act_nr_PER_case_id_ON_act_n_PER_case_id = 
        act_nr_PER_case_id / act_n_PER_case_id * 100,
      
      ## Rate of activity number per case and activity level
      ## ... on total activities per case
      act_nr_PER_case_id_and_act_level_ON_act_n_PER_case_id = 
        act_nr_PER_case_id_and_act_level / act_n_PER_case_id * 100,
      
      ## Rate of activity number per case and activity level
      ## ... on total activities per activity level
      act_nr_PER_case_id_and_act_level_ON_act_n_PER_act_level = 
        act_nr_PER_case_id_and_act_level / act_n_PER_act_level * 100,
      
      ## Rate of activity number per case and activity level
      ## ... on total activities
      act_nr_PER_case_id_and_act_level_ON_act_n = 
        act_nr_PER_case_id_and_act_level / act_n * 100,
      
      ## Rate of activity number per case
      ## ... on total activities
      act_nr_PER_case_id_ON_act_n = 
        act_nr_PER_case_id / act_n * 100,
      
      ## Rate of activity number per activity level
      ## ... on total activities per activity level
      act_nr_PER_act_level_ON_act_n_PER_act_level = 
        act_nr_PER_act_level / act_n_PER_act_level * 100,
      
      ## Rate of activity number per activity level
      ## on total activities
      act_nr_PER_act_level_ON_act_n = 
        act_nr_PER_act_level / act_n * 100)
  
  
  
  # Compute ratio for first activities --------------------------------------
  
  ## Result won't end up at 100 % for each activity level
  ## ... as not necessarily all cases show these activity levels
  
  dat_n_1st_ratio <- 
    dat_n_1st %>% mutate(
      
      ## Rate of 1st activity number per activity level 
      ## ... on total 1st activities per activity level 
      act_nr_PER_act_level_1st_ON_act_n_PER_act_level_1st =
        act_nr_PER_act_level_1st / act_n_PER_act_level_1st * 100,
      
      ## Rate of 1st activity number per activity level
      ## ... on total activities per activity level
      act_nr_PER_act_level_1st_ON_act_n_PER_act_level =
        act_nr_PER_act_level_1st / act_n_PER_act_level * 100,
      
      ## Rate of 1st activity number per activity level
      ## ... on total activities
      act_nr_PER_act_level_1st_ON_act_n = 
        act_nr_PER_act_level_1st / act_n * 100,
      
      ## Rate of 1st activity number per activity level
      ## ... on total cases
      act_nr_PER_act_level_1st_ON_cases_n = 
        act_nr_PER_act_level_1st / cases_n * 100
    )
  
  
  
  # LOCF over longitudinal reference for all activities ---------------------
  
  dat_full <- 
    extendActivityRatesOnFullData(
      dat_n_ratio,
      col_name_am = col_name_am,
      col_name_ref_related = "am_min_PER_act_id",
      col_name_act_level = col_name_act_level,
      col_names_ratio_related = grep("ON", names(dat_n_ratio), value = T),
      dat_long_ref = template,
      unique_level
    )
  
  
  
  # LOCF over longitudinal reference for first activities -------------------
  
  dat_full_1st <- 
    extendActivityRatesOnFullData(
      dat_n_1st_ratio,
      col_name_am = col_name_am,
      col_name_ref_related = "am_min_PER_case_id_and_act_level",
      col_name_act_level = col_name_act_level,
      col_names_ratio_related = grep("ON", names(dat_n_1st_ratio), value = T),
      dat_long_ref = template,
      unique_level
    )
  
  
  
  # Return list of data frames ----------------------------------------------
  
  dat_return <- list(dat             = data.frame(dat),
                     dat_am          = data.frame(dat_am),
                     dat_n           = data.frame(dat_n),
                     dat_n_1st       = data.frame(dat_n_1st),
                     dat_n_ratio     = data.frame(dat_n_ratio),
                     dat_n_1st_ratio = data.frame(dat_n_1st_ratio),
                     dat_full        = data.frame(dat_full),
                     dat_full_1st    = data.frame(dat_full_1st))
  
  outputDone()
  return(dat_return)
}
