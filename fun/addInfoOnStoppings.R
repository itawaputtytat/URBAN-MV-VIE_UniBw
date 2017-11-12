addInfoOnStoppings <- function(dat, 
                               db_conn_name,
                               db_src_name,
                               col_name_case = "passing") {
  
  outputFunProc(R)
  
  obj_name <- deparseDataFunArg(dat, return_dat = F)
  dat <- deparseDataFunArg(dat)
  
  ## Get stoppings data
  dat_stoppings <- dbGetSrc(db_conn_name, db_src_name)
  
  ## Select only relevant columns
  dat_stoppings <- 
    dat_stoppings %>% 
    select_(col_name_case, "is_stopping")
  
  ## Rename "is_stopping" corresponding to table name
  col_name_new <- db_src_name
  col_name_new <- sub("t_", "", col_name_new)
  col_name_new <- sub("stoppings", "is_stopping", col_name_new)
  
  col_finder <- colnames(dat_stoppings) == "is_stopping"
  colnames(dat_stoppings)[col_finder] <- col_name_new
  
  dat <- 
    left_join(dat,
              dat_stoppings,
              by = col_name_case)
  
  assign(obj_name, dat, env = .GlobalEnv)
  
}