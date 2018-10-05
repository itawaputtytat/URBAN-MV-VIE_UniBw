
predLiebner_compProb_al_Mk <- function(dat, 
                                       col_name_id = "passing",
                                       col_name_cluster_group = "cluster_group", 
                                       col_name_acc_lon_max = "acc_lon_ms2_max",
                                       col_name_group = NULL,
                                       thresholds_acc,
                                       return_assignments = F) {
  
  ## In case of no grouping variable
  col_name_group_init <- col_name_group
  if (is.null(col_name_group)) {
    col_name_group <- "dummy"
    dat[, col_name_group] <- 1
  }
  
  ## Select relevant columns
  dat <- 
    dat %>% 
    distinct_(.dots = c(
      col_name_id, 
      col_name_cluster_group, 
      col_name_group,
      col_name_acc_lon_max))
  
  ## Assign groups based on thresholds
  dat[, "a"] <- NA
  for (i in 1:length(thresholds_acc)) {
    row_finder <- 
      is.na(dat[, "a"]) & dat[, col_name_acc_lon_max] <= 
      thresholds_acc[i]
    dat[row_finder, "a"] <- paste0("l", i)
  }
  
  ## Assign an additional category for values exceeding the last threshold
  row_finder <- is.na(dat[, "a"])
  dat[row_finder, "a"] <- paste0("l", length(thresholds_acc) + 1)
  
  dat[, "a"] <- factor(dat[, "a"], levels = paste0("l", 1:(2 + 1)))
  
  ## Compute rate per cluster group and threshold
  ## Sort values by M and a 
  dat <- 
    dat %>% 
    group_by_(.dots = c(col_name_cluster_group, col_name_group, "a")) %>% 
    mutate(count_per_group = n()) %>% 
    group_by_(.dots = c(col_name_cluster_group, col_name_group)) %>% 
    mutate(rate = count_per_group / n()) %>% 
    arrange_(col_name_cluster_group, "a") %>%
    ungroup() %>% 
    complete_(cols = c(col_name_cluster_group, col_name_group, "a"), fill = list(rate = 0)) %>% 
    distinct_(.dots = c(col_name_cluster_group, col_name_group, "a", "rate")) 
  
  ## Correct values to uniform distribution if zero
  dat <- 
    dat %>% 
    group_by_(.dots = c(col_name_cluster_group, col_name_group)) %>% 
    mutate(rate_sum = sum(rate)) %>% 
    mutate(count_temp = n()) %>% 
    ungroup() %>% 
    mutate(rate = ifelse(rate_sum == 0, 1/count_temp, rate)) %>% 
    mutate(rate_sum = NULL,
           count_temp = NULL)
  
  ## Rename cluster group column
  dat <- 
    dat %>% 
    rename_at(col_name_cluster_group, funs(paste0("M"))) %>% 
    mutate(M = paste("k", M, sep = "")) %>% 
    data.frame()
  
  ## Remove dummy group in case of no grouping variable
  if (is.null(col_name_group_init)) {
    dat[, "dummy"] <- NULL
  }
  
  return(dat)
}
