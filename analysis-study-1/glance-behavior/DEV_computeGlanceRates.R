
# Objective ---------------------------------------------------------------
## Compute glance rates for each direction in two versions
## 1) As percentage of first glances
## 2) As percentage of overall glances

## Important: Must use glance_dir variable without "_txt"
## .. and as conversion to factor



# Start of function -------------------------------------------------------

computeGlanceRates <- function(dat, 
                               col_name_act_level, 
                               col_name_am,
                               col_name_case_id,
                               col_name_time = "time_s") {
  
  outputFunProc(R)
  
  
  
  # Preparatory settings ----------------------------------------------------
  
  
  ## Create new variables to avoid usage of dynamic variables
  ## Distance variable ill be renamed at the end of function
  dat$xdir <- dat[, col_name_act_level]
  dat$xdist <- dat[, col_name_am]
  
  ## Remove missing values
  dat <-
    dat %>%
    mutate(xdir = ifelse(is.na(xdir), 0, xdir))
  
  ## Factorize glance directions
  glance_labels <- c("undefined",
                     "front",
                     "combi",
                     "rear",
                     "left",
                     "right")
  dat[, "xdir"] <- factor(dat[, "xdir"],
                          levels = c(0,1,2,3,4,6),
                          labels = glance_labels)
  
  ## Detect number of cases with available glance data
  ## 1) Flter rows with glance data
  ## Counting by distinct case values
  n_cases_w_glance_data <- 
    dat %>% 
    filter(!is.na(xdir))
  n_cases_w_glance_data <- 
    length(unique((n_cases_w_glance_data[, col_name_case_id])))
  #n_cases_w_glance_data <- 29
  
  cases_n <- n_cases_w_glance_data
  
  ## Create template for distances (and rounds)
  ## Will be used for data completation after computations of rates
  #template <- distinct(select(dat, round_txt, xdist))
  template <- 
    dat %>% 
    #select_(col_name_case_id, col_name_am) %>% 
    select_(col_name_am) %>% 
    distinct() %>% 
    arrange_(col_name_am)
  
  ## Initiliase unique glance directions (dependent on type selection)
  ## Will be used for data completion (each direction gets own column)
  unique_act_level <- unique(dat$xdir)
  unique_act_level <- c(0,1,2,3,4,6)
  
  
  # Preparatory data cleaning -----------------------------------------------
  
  dat_glances_seq <- dat %>%
    
    ## Remove observations where glance_dir == NA
    ## Order by case and time
    ## Create row numbers over complete data frame
    filter(!is.na(xdir)) %>%
    select_(col_name_am,
            col_name_time,
            col_name_case_id,
            col_name_act_level,
            "xdir",
            "xdist") %>% 
    arrange_(col_name_case_id, col_name_time) %>%
    mutate(row_nr = row_number())
  
  
  # Create id for each glance sequence --------------------------------------
  
  dat_glances_seq <- dat_glances_seq %>%
    
    ## Create row numbers within each xdir, case
    ## Create id for current glance sequence
    group_by_(col_name_case_id, "xdir") %>%
    mutate(glance_row_nr = row_number()) %>%
    mutate(glance_id = (row_nr - glance_row_nr)) %>% 
    mutate(row_nr = NULL,
           glance_row_nr = NULL)
  
  ## In case of uncertainty call following line for proofing uniqueness of ids
  ## View(dat %>%
  ## group_by(subject_id, glance_id)  %>%
  ## summarise(n_unique_glance_ids = length(unique(glance_id))))
  
  
  
  # New variables until here ------------------------------------------------
  
  ## row_nr: row numbers of complete dataframe
  ## glance_row_nr: row number within each glance direction within case
  
  
  
  # Computations related to driven distance ---------------------------------
  
  dat_glances_seq <- 
    dat_glances_seq %>%
    
    ## Minimum driven distance for each single glance within each case
    group_by(glance_id) %>%
    mutate(am_min_per_case_id_and_glance_id = min(xdist)) %>%
    
    ## Minimum driven distance for each glance direction within each case
    ## = Minimum distance of first glance in this direction
    group_by_(col_name_case_id, "xdir") %>%
    mutate(am_min_per_case_id_and_glance_code = min(xdist)) %>%
    
    ## Minimum driven distance for each glance direction
    ## Independent from current case
    ## = Overall minimum driven distance of first glances in current direction
    group_by(xdir) %>%
    mutate(am_min_per_glance_code = min(am_min_per_case_id_and_glance_code))
  
  dat_glances_seq2 <- dat_glances_seq[, c(1,2,3,7,4)]
  names(dat_glances_seq2) <- 
    c("am",
      "time_s",
      "case_id",
      "act_id",
      "act_level")
  
  dat_am <- 
    dat_glances_seq2 %>%
    group_by(act_id) %>% 
    mutate(am_min_PER_act_id = min(am)) %>% 
    group_by(case_id, act_level) %>% 
    mutate(am_min_PER_case_id_and_act_level = min(am_min_PER_act_id)) %>%
    group_by(act_level) %>% 
    mutate(am_min_PER_act_level = min(am_min_PER_act_id)) %>%
    group_by(case_id, act_id, act_level) %>% 
    summarise(am_min_PER_act_id = min(am_min_PER_act_id),
              am_min_PER_case_id_and_act_level = min(am_min_PER_case_id_and_act_level),
              am_min_PER_act_level = min(am_min_PER_act_level)) %>% 
    arrange(case_id, am_min_PER_act_id)
  
  
  
  # New variables until here ------------------------------------------------
  
  ## row_nr: row numbers of complete dataframe
  ## glance_row_nr: row number within each glance direction within case
  
  ## glance_id: unique sequence identifier
  ## am_min_per_case_id_and_glance_id: minimum driven distance within each sequence
  ## am_min_per_case_id_and_glance_code: minimum driven distance within each direction
  ## am_min_per_glance_code: overall minimum driven distance within each direction
  
  
  
  # Computations related to quantity of glances -----------------------------
  
  dat_glances_seq <- 
    dat_glances_seq %>%
    
    ## Count overall glances within each case
    group_by_(col_name_case_id) %>%
    mutate(n_glances_per_case_id = length(unique(glance_id))) %>%
    
    ## Count glances for each glance direction within each case
    group_by_(col_name_case_id, "xdir") %>%
    mutate(n_glances_per_case_id_and_glance_code = length(unique(glance_id))) %>%
    
    ## Count overall glances for each glance direction
    ## Independent from each case
    group_by_("xdir") %>%
    mutate(n_glances_per_glance_code = length(unique(glance_id))) %>%
    
    ## Count all first glances for each glance direction
    ## ... at each minimum distance ...
    ## ... (coming from within each case)
    ## = Number of 1st glances done at this distance
    ## = How many subject_ids also looked (already) in this direction
    ## (at exactly this position)
    group_by(xdir, am_min_per_case_id_and_glance_code) %>%
    mutate_(.dots = setNames(list(interp(~ length(unique(v)), 
                                        v = as.name(col_name_case_id))), 
                            "n_1st_glances_per_min_am")) %>% 
    
    ## Count overall glances
    ungroup() %>%
    mutate(n_glances = length(unique(glance_id)))
  
  dat_n <-
    dat_glances_seq2 %>% 
    mutate(act_n = length(unique(act_id))) %>% 
    group_by(case_id) %>% 
    mutate(act_n_PER_case_id = length(unique(act_id))) %>%
    group_by(case_id, act_level) %>%
    mutate(act_n_PER_case_id_and_act_level = length(unique(act_id))) %>%
    group_by(act_level) %>%
    mutate(act_n_PER_act_level = length(unique(act_id))) %>% 
    group_by(case_id, act_level) %>% 
    summarise(act_n_PER_case_id = min(act_n_PER_case_id),
              act_n_PER_case_id_and_act_level = min(act_n_PER_case_id_and_act_level),
              act_n_PER_act_level = min(act_n_PER_act_level),
              act_n = min(act_n)) %>% 
    select(case_id, 
           act_n_PER_case_id, 
           act_level, 
           act_n_PER_case_id_and_act_level,
           act_n_PER_act_level,
           act_n) %>% 
    arrange(case_id,
            act_n_PER_case_id)
  
  
  dat_n_1st <-
    dat_am %>% 
    group_by(act_level, am_min_PER_case_id_and_act_level) %>%
    summarise_(case_ids = "paste(unique(case_id), collapse = \";\")",
               .dots = setNames(list(interp(~ length(unique(v)), 
                                           v = as.name("case_id"))), 
                               "act_n_1st_PER_am_min")) %>% 
    select(am_min_PER_case_id_and_act_level,
           act_level, 
           act_n_1st_PER_am_min,
           case_ids) %>% 
    arrange(am_min_PER_case_id_and_act_level)
  
  
  # New variables until here ------------------------------------------------
  
  ## row_nr: row numbers of complete dataframe
  ## glance_row_nr: row number within each glance direction within case
  ##
  ## glance_id: unique sequence identifier
  ## am_min_per_case_id_and_glance_id: minimum driven distance within each sequence
  ## am_min_per_case_id_and_glance_code: minimum driven distance within each direction
  ## am_min_per_glance_code: overall minimum driven distance within each direction
  ##
  ## n_glances_per_case_id_and_glance_code: number of glances in direction within subject
  ## n_glances_per_case_id: number of all glances within subject
  ## n_glances_per_glance_code: overall number of glances within each direction
  ## n_1st_glances_per_min_am: number of glances
  ## n_glances: overall number of glances
  
  
  
  # Computations for relative rates of glances ------------------------------
  
  dat_glances_seq <- 
    dat_glances_seq %>%
    
    ## Rate of glances-number of each subject in current direction ...
    ## ... on all glances (in any direction) of this subject
    mutate(n_glances_per_case_id_ON_n_glances_per_case_id_and_glance_code =
             100 / n_glances_per_case_id * n_glances_per_case_id_and_glance_code) %>%
    
    ## Rate of glances of each subject in current direction ...
    ## ... on all glances in current direction (indepecent from subject)
    mutate(n_glances_per_glance_code_ON_n_glances_per_case_id_and_glance_code =
             100 / n_glances_per_glance_code * n_glances_per_case_id_and_glance_code) %>%
    
    ## Rate of glances of each subject in current direction ...
    ## ... on all glances (in any direction = independent from direction)
    ## ... and independet from from subject
    mutate(n_glances_ON_n_glances_per_case_id_and_glance_code =
             100 / n_glances * n_glances_per_case_id_and_glance_code) %>%
    
    ## Rate of overall glances of each subject ...
    ## ... on all glances (from all subjects, in all directions)
    mutate(n_glances_ON_n_glances_per_case_id =
             100 / n_glances * n_glances_per_case_id) %>%
    
    ## Rate of glances of each subject_id-distmin-glance in current direction
    ## ... on all glances in current direction
    mutate(n_glances_per_glance_code_ON_n_1st_glances_per_min_am =
             100 / n_glances_per_glance_code * n_1st_glances_per_min_am) %>%
    
    ## Rate of glances of each subject_id-distmin-glance in current direction ...
    ## ... on all subjects with glance data
    ## = 1st glance percentage
    mutate(n_cases_w_glance_data_ON_n_1st_glances_per_min_am =
             100 / n_cases_w_glance_data * n_1st_glances_per_min_am)
  
  dat_ratio <- 
    dat_n %>% 
    mutate(act_n_PER_case_id_and_act_level_ON_act_n_PER_case_id =
             100 / act_n_PER_case_id * act_n_PER_case_id_and_act_level) %>% 
    mutate(act_n_PER_case_id_and_act_level_ON_act_n_PER_act_level =
             100 / act_n_PER_act_level * act_n_PER_case_id_and_act_level) %>% 
    mutate(act_n_PER_case_id_and_act_level_ON_act_n =
             100 / act_n * act_n_PER_case_id_and_act_level) %>% 
    mutate(act_n_PER_case_id_ON_act_n =
             100 / act_n * act_n_PER_case_id) %>% 
    arrange(case_id, act_level)
  
  dat_ratio_1st <- 
    left_join(dat_n_1st,
              dat_n %>% 
                group_by(act_level) %>% 
                summarise(act_n_PER_act_level = min(act_n_PER_act_level)),
              by = "act_level") %>% 
    mutate(act_n_1st_PER_am_min_ON_act_n_PER_act_level =
             100 / act_n_PER_act_level * act_n_1st_PER_am_min) %>% 
    mutate(act_n_1st_PER_am_min_ON_cases_n =
             100 / cases_n * act_n_1st_PER_am_min) %>% 
    arrange(am_min_PER_case_id_and_act_level)
  
  
  # New variables until here ------------------------------------------------
  
  ## row_nr: row numbers of complete dataframe
  ## glance_row_nr: row number within each glance direction within subject
  ##
  ## glance_id: unique sequence identifier
  ## am_min_per_case_id_and_glance_id: minimum driven distance within each sequence
  ## am_min_per_case_id_and_glance_code: minimum driven distance within each direction
  ## am_min_per_glance_code: overall minimum driven distance within each direction
  ##
  ## n_glances_per_case_id_and_glance_code: number of glances in direction within subject
  ## n_glances_per_case_id: number of all glances within subject
  ## n_glances_per_glance_code: overall number of glances within each direction
  ## n_1st_glances_per_min_am: number glances
  ## n_glances: overall number of glances
  ##
  ## n_glances_per_case_id..n_glances_per_case_id_and_glance_code
  ## n_glances_per_glance_code..n_glances_per_case_id_and_glance_code
  ## n_glances..n_glances_per_case_id_and_glance_code
  ## n_glances..n_glances_per_case_id
  ## n_glances_per_glance_code..n_1st_glances_per_min_am
  ## n_cases_w_glance_data..n_1st_glances_per_min_am
  
  
  
  # Compute cumulative values for 1st glances -------------------------------
  
  ## Result won't end up at 100 % for each activity level
  ## ... as not necessarily all cases show these activity levels
  
  dat_glances1st_cum <- dat_glances_seq %>%
    
    ## Group by minimum distance of first glances
    group_by(xdir, am_min_per_case_id_and_glance_code) %>%
    
    ## Result: Relative amount of glances on all first glances
    summarise(n_cases_w_glance_data_ON_n_1st_glances_per_min_am =
                min(n_cases_w_glance_data_ON_n_1st_glances_per_min_am)) %>%
    
    ## Cumulate the computed value from above
    mutate(n_cases_w_glance_data_ON_n_1st_glances_per_min_am__cum =
             cumsum(n_cases_w_glance_data_ON_n_1st_glances_per_min_am))
  
  dat_ratio_1st_cum <-
    dat_ratio_1st %>%
    group_by(act_level) %>%
    arrange(am_min_PER_case_id_and_act_level) %>% 
    #group_by(act_level, am_min_PER_case_id_and_act_level) %>% 
    # summarise(act_n_1st_PER_am_min_ON_cases_n = 
    #             min(act_n_1st_PER_am_min_ON_cases_n)) %>% 
    mutate(act_n_1st_PER_am_min_ON_cases_n__CUM =
             cumsum(act_n_1st_PER_am_min_ON_cases_n)) %>% 
    arrange(act_level, am_min_PER_case_id_and_act_level)
  
  
  # Compute cumulative values for all glances -------------------------------
  
  
  dat_ratio_cum <- dat_glances_seq %>%
    
    ## Group by case, activity category, and activity id 
    group_by_(col_name_case_id, "xdir", "glance_id") %>%
    
    ## Extract minimum distance for each activity
    ## Extract total number of glances (either using min or max)
    summarize(am_min_per_case_id_and_glance_id = min(xdist),
              n_glances = min(n_glances)) %>%
    
    ## Group by direction
    group_by(xdir) %>%
    
    ## Re-order data by minimum distance for each  activity
    ## Add new row number within each direction
    arrange(am_min_per_case_id_and_glance_id) %>%
    mutate(row_nr_within_glance_dir = row_number()) %>%
    
    ## Group by activity category and minimum distance of each activity
    group_by(xdir, am_min_per_case_id_and_glance_id) %>%
    
    ## Compute maximum number of glances
    mutate(n_glances_per_glance_code_max = max(row_nr_within_glance_dir)) %>%
    ## Compute percentage on total number of glances
    mutate(n_glances_ON_n_glances_per_glance_code_max =
             100 / n_glances * n_glances_per_glance_code_max) %>%
    
    ## While aggregating to minimum distance of each sequence ...
    ## ... remember total number of glances
    ## ... compute maximum number of glances up to each am_min_per_case_id_and_glance_id
    ## ...
    summarize(n_glances = max(n_glances),
              n_glances_per_glance_code_max = 
                max(n_glances_per_glance_code_max),
              n_glances_ON_n_glances_per_glance_code_max = 
                max(n_glances_ON_n_glances_per_glance_code_max))
  
  dat_ratio_cum <-
    dat_am %>% 
    ungroup() %>% 
    mutate(act_n = length(unique(act_id))) %>% 
    arrange(am_min_PER_act_id) %>% 
    group_by(act_level) %>% 
    mutate(act_id_PER_act_level = row_number()) %>% 
    group_by(act_level, am_min_PER_act_id) %>% 
    mutate(act_id_PER_act_level_MAX = max(act_id_PER_act_level)) %>% 
    mutate(act_id_PER_act_level_MAX_ON_act_n = 
             100 / act_n * act_id_PER_act_level_MAX) %>% 
    summarise(act_n = max(act_n),
              act_id_PER_act_level_MAX = max(act_id_PER_act_level_MAX),
              act_id_PER_act_level_MAX_ON_act_n_CUM = max(act_id_PER_act_level_MAX_ON_act_n)) %>% 
    arrange(act_level, am_min_PER_act_id)
  
  # Create complete datasets of distance ------------------------------------
  
  ## Objective:
  ## Create complete (for distances) dataframe
  ## = Merge with complete-distance-template
  ## Will create new column for each glance direction
  
  
  
  # ... for 1st glances -----------------------------------------------------
  
  # dat_glances1st_full <-
  #   glanceDataRatesCompletion(dir_unique, dat_glances1st_cum, template,
  #                             col_name_am,
  #                             "am_min_per_case_id_and_glance_code")
  
  dat_1st_full <- 
    glanceDataRatesCompletion(dat_ratio_1st_cum,
                              col_name_ref_related = "am_min_PER_case_id_and_act_level",
                              col_names_ratio_related = 
                                c("act_n_1st_PER_am_min_ON_act_n_PER_act_level",
                                  "act_n_1st_PER_am_min_ON_cases_n__CUM"),
                              dat_long_ref = template)
  
  # ## Rename distance variable
  # colnames_old <- colnames(dat_glances1st_full)
  # colnames_new <- colnames_old
  # colnames_new[which(colnames_new == "xdist")] <- col_name_am
  # colnames(dat_glances1st_full) <- colnames_new
  
  
  ## Check for missing variables (due to no activity in levels)
  ## missing glance directions will set to 0)
  col_names_to_check <- c()
  for (act_level in unique_act_level) {
    #for(label in glance_labels) {
    
    ## Create template for each glance direction
    col_names_to_check <-
      c(col_names_to_check,
        
        # ## Create colnames from xdir-levels
        # paste("n_cases_w_glance_data", c(
        #   paste("_ON_n_1st_glances_per_min_am__", act_level, sep = ""),
        #   paste("_ON n_1st_glances_per_min_am__cum__", act_level, sep = "")), sep = "")
        ## Create colnames from xdir-levels
        paste("act_n_1st_PER_am_min_ON_", c(
          paste("act_n_PER_act_level", act_level, sep = "__"),
          paste("cases_n__CUM", act_level, sep = "__")), sep = "")
      ) 
  }
  
  ## Find missing columns
  val_finder <- !which(col_names_to_check %in% names(dat_1st_full))
  col_names_missing <- col_names_to_check[val_finder]
  
  ## Add column with zeros for all missing columns
  for (col_name in col_names_missing) {
    
    colnames_old <- names(dat_1st_full)
    colnames_new <- c(names(dat_1st_full), col_name)
    dat_1st_full <- cbind(dat_1st_full, rep(0, nrow(dat_1st_full)))
    names(dat_1st_full) <- colnames_new
  }
  
  ## Create consistent column order
  col_names_order <- c(col_name_am, col_names_to_check)
  dat_1st_full <- dat_1st_full[, col_names_order]
  
  
  
  # Create complete dataset over distance for all glances -------------------
  
  # dat_glances_full <-
  #   glanceDataRatesCompletion(dir_unique, dat_glances_cum, template,
  #                             "am_min_per_case_id_and_glance_id")
  
  dat_full <- 
    glanceDataRatesCompletion(dat_ratio_cum,
                              col_name_ref_related = "am_min_PER_act_id",
                              col_names_ratio_related = 
                                c("act_id_PER_act_level_MAX",
                                  "act_id_PER_act_level_MAX_ON_act_n_CUM"),
                              dat_long_ref = template)
  
  # Rename distance variable
  # colnames_old <- colnames(dat_glances_full)
  # colnames_new <- colnames_old
  # colnames_new[which(colnames_new == "xdist")] <- col_name_am
  # colnames(dat_glances_full) <- colnames_new
  # 
  
  ## Check for missing variables (= missing glance directions will set to 0)
  col_names_to_check <- c()
  
  for (act_level in unique_act_level) {
    
    ## Create template for each glance direction
    col_names_to_check <-
      c(col_names_to_check,
        
        ## Create colnames from xdir-levels
        # paste("n_glances", c(
        #   paste("__", act_level, sep = ""),
        #   paste(".dir__max__", act_level, sep = ""),
        #   paste("..n_glances_per_glance_code__max__", act_level, sep = "")), sep = "")
        paste("act_id_PER_act_level_MAX", c(
          paste("", act_level, sep = "__"),
          paste("_ON_act_n_CUM", act_level, sep = "__")), sep = "")
      ) ## End of vector: col_names_to_check
    
  }
  
  ## Find missing columns
  val_finder <- !which(col_names_to_check %in% names(dat_full))
  col_names_missing <- col_names_to_check[val_finder]
  
  ## Add column with zeros for all missing columns
  for (col_name in col_names_missing) {
    
    colnames_old <- names(dat_full)
    colnames_new <- c(names(dat_full), col_name)
    dat_full <- cbind(dat_full, rep(0, nrow(dat_full)))
    names(dat_full) <- colnames_new
  }
  
  ## Create consistent column order
  col_names_order <- c(col_name_am, col_names_to_check)
  dat_full <- dat_full[, col_names_order]
  
  
  
  
  
  ## Return list of data frames
  
  dat_return <- list(dat    = data.frame(dat),
                     dat_am = data.frame(dat_am),
                     dat_n   = data.frame(dat_n),
                     dat_n_1st = data.frame(dat_n_1st),
                     dat_ratio = data.frame(dat_ratio),
                     dat_ratio_1st = data.frame(dat_ratio_1st),
                     dat_ratio_cum = data.frame(dat_ratio_cum),
                     dat_ratio_1st_cum = data.frame(dat_ratio_1st_cum),
                     dat_full = data.frame(dat_full),
                     dat_1st_full = data.frame(dat_1st_full))
  
  
  return(dat_return)
  outputDone()
  
}