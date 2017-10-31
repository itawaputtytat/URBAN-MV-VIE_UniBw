
# Objective ---------------------------------------------------------------

## Create complete (for distances) dataframe
## = Merge with complete-distance-template
## Will create new column for each glance direction

glanceDataRatesCompletion <- function(dat,
                                      col_name_ref_related,
                                      col_name_act_level = "act_level",
                                      col_names_ratio_related,
                                      dat_long_ref = template) { 
  
  outputFunProc(R)
  
  dat <- 
    dat %>% 
    select(col_name_ref_related,
           col_name_act_level,
           col_names_ratio_related) %>% 
    data.frame()
  
  # ## Workaround for joining
  # dat_long_ref[, names(dat_long_ref)] <- 
  #   as.character(dat_long_ref[, names(dat_long_ref)])
  # 
  # 
  # dat[, col_name_ref_related] <- 
  #   as.character(dat[, col_name_ref_related])
  # 
  # print(str(dat))
  # pauseAndContinue()
  
  ## Extract unique activity level
  unique_act_level <- unique(dat[, col_name_act_level])
  
  ## Initialise collector based on longitudinal reference data
  dat_coll <- dat_long_ref
  
  ## Loop through activity levels
  for (act_level in unique_act_level) {
    
    ## Get temporary data for current activity level
    row_finder <- which(dat[, col_name_act_level] == act_level)
    dat_temp <- dat[row_finder, ]
    
    ## Join template and temporary data for currenct activity level
    # dat_joined <-
    #   left_join(dat_long_ref,
    #             dat_temp,
    #             by = setNames(col_name_ref_related, names(dat_long_ref)))
    
    dat_joined <-
      left_join(dat_long_ref,
                dat_temp,
                by = setNames(col_name_ref_related, names(dat_long_ref)))
    
    ## Delete column for glance direction
    ## ... because specific column names created in next step
    ## (we need a extra column for each direction)
    ## It's safe to delete, because there is an extra loop for each direction
    dat_joined[, col_name_act_level] <- NULL
    
    ## Create specific column names for each direction
    ## Get number of columns
    ## Select and rename columns to be renamed
    col_names_ratio_related_new <- 
      c(paste(col_names_ratio_related, act_level, sep = "__"))
    ## Rename columns
    col_finder <- which(names(dat_joined) %in% col_names_ratio_related)
    names(dat_joined)[col_finder] <- col_names_ratio_related_new
    
    ## Fill NA-values with previous values (otherwise replace with 0)
    dat_joined <- na.locf(dat_joined)
    dat_joined[is.na(dat_joined)] <- 0
    
    ## Merge template and data from rounds (with all directions)
    dat_coll <- 
      left_join(dat_coll,
                dat_joined,
                by = names(dat_long_ref))    
    
  } # End of loop through directions
  
  # dat_coll[, names(dat_long_ref)] <-
  #   as.numeric(dat_coll[, names(dat_long_ref)])
  
  return(dat_coll)
  
} # End of function ----
