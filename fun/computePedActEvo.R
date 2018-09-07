computePedActEvo <- function(dat,
                             dat_template,
                             col_name_id,
                             col_names_group,
                             col_name_am,
                             col_name_value) {
  
  ## Add group information
  col_names_group_all <- c(col_name_id, col_names_group)
  dat_new <-
    left_join(dat,
              dat_template %>% 
                select_(.dots = col_names_group_all) %>% 
                distinct())
  
  ## Select variables
  dat_new <- 
    dat_new %>% 
    select_(col_name_id,
            col_names_group,
            col_name_value)
  
  ## Replace NA
  rowfinder <- is.na(dat_new[, col_name_value])
  dat_new[rowfinder, col_name_value] <- min(dat_template[, col_name_am])
  
  ## Compute percentage
  dat_new <- 
    dat_new %>% 
    group_by_(col_names_group) %>%
    arrange_(paste_(col_name_value)) %>%
    mutate(percentage = row_number() / n() * 100) %>% 
    group_by_(col_names_group, 
              col_name_value) %>% 
    summarise(percentage = max(percentage)) %>% 
    rename_(.dots = setNames(col_name_value, col_name_am)) %>% 
    data.frame()
  
  ## Complete percentage
  dat_new <- 
    dat_template %>% 
    select_(.dots = c(col_name_am, col_names_group)) %>% 
    distinct() %>% 
    left_join(dat_new) %>% 
    group_by_(col_names_group) %>% 
    mutate(percentage = na.locf(percentage, na.rm = FALSE)) %>% 
    mutate(percentage = ifelse(is.na(percentage), 0, percentage)) %>% 
    data.frame()
}