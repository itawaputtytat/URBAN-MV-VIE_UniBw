predLiebner_compProb_Mk <- function(dat,
                                    col_name_id = "passing",
                                    col_name_cluster_group = "cluster_group", 
                                    col_name_group = NULL) {

  ## In case of no grouping variable
  col_name_group_init <- col_name_group
  if (is.null(col_name_group)) {
    col_name_group <- "dummy"
    dat[, col_name_group] <- 1
  }

  dat <- 
    dat %>% 
    rename_at(col_name_cluster_group, funs(paste0("M"))) %>% 
    #mutate_(M = col_name_cluster_group) %>% 
    mutate(M = paste("k", M, sep = "")) %>% 
    distinct_(.dots = c(col_name_id, col_name_group, "M")) %>% 
    group_by_(.dots = c(col_name_group, "M")) %>%
    summarise(count = n()) %>%
    ungroup() %>% 
    complete_(cols = c(col_name_group, "M"), fill = list(count = 0)) %>% 
    group_by_(.dots = c(col_name_group)) %>% 
    mutate(count_group = sum(count)) %>%  
    mutate(rate = count/count_group) %>% 
    # mutate(rate = count/sum(count)) %>% 
    data.frame()
  
  ## Remove dummy group in case of no grouping variable
  if (is.null(col_name_group_init)) {
    dat[, "dummy"] <- NULL
  }
  
  return(dat)
}


# dat_Mk %>% 
#   rename_at("cluster_group_ordered", funs(paste0("M"))) %>% 
#   mutate(M = paste("k", M, sep = "")) %>% 
#   distinct_(.dots = c("passing", "condition_speed", "M")) %>% 
#   group_by_(.dots = c("condition_speed", "M")) %>%
#   summarise(count = n()) %>%
#   ungroup() %>% 
#   complete_(cols = c("condition_speed", "M"), fill = list(count = 0)) %>% 
#   group_by_(.dots = c("condition_speed")) %>% 
#   mutate(count_group = sum(count)) %>%  
#   mutate(rate = count/count_group)

  