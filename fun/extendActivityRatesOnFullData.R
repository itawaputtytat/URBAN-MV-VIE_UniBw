
## Create complete data set over range of longitudinal reference
## (by use of template)
## Will create new column for each activity level

extendActivityRatesOnFullData <- function(dat,
                                          col_name_am,
                                          col_name_ref_related,
                                          col_name_act_level,
                                          col_names_ratio_related,
                                          dat_long_ref = template,
                                          unique_level
                                          ) { 
  
  outputFunProc(R)
  
  ## Data preparation
  dat <- 
    dat %>% 
    select(col_name_ref_related,
           col_name_act_level,
           col_names_ratio_related) %>% 
    data.frame()
  
  ## Extract unique activity level available in current data
  ## Ignore given data in unique_level
  unique_act_level_in_dat <- unique(dat[, col_name_act_level])
  
  ## Initialise collector based on longitudinal reference data
  dat_coll <- dat_long_ref
  
  
  ## Loop through activity levels
  for (act_level in unique_act_level_in_dat) {
    
    ## Get temporary data for current activity level
    row_finder <- which(dat[, col_name_act_level] == act_level)
    
    dat_temp <- dat[row_finder, ]
  
    dat_joined <-
      left_join(dat_long_ref,
                dat_temp,
                by = setNames(col_name_ref_related, names(dat_long_ref)))
    
    ## Delete column for activity level
    ## ... because extra column will be created for each activity level
    dat_joined[, col_name_act_level] <- NULL
    
    ## Create specific column names for each activity level
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
    
  } # End of loop through activity level
  
  ## In case there was no value for one of the pre-defined levels
  ## ... am additional column will be created with every row being 0
  col_names_to_check <-
    lapply(unique_level,
           function(x)
             paste(col_names_ratio_related, x, sep = "__"))
  col_names_to_check <- unlist(col_names_to_check)
  
  ## Find missing columns
  col_names_missing <- setdiff(col_names_to_check, names(dat_coll))
  
  ## Add column with zeros for all missing columns
  for (col_name in col_names_missing) {
    dat_coll[, col_name] <- 0
  }
  
  ## Create consistent column order
  col_names_order <- c(col_name_am, col_names_to_check)
  dat_coll <- dat_coll[, col_names_order]
  
  return(dat_coll)
  
} 
