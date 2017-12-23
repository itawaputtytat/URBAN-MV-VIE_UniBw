ggplot2_legendJustification <- 
  function(pos_name_h,
           pos_name_v) {
    
    ## Lower case
    pos_name_h <- tolower(pos_name_h)
    pos_name_v <- tolower(pos_name_v)
    
    ## Assign values
    pos_h <- ifelse(substr(pos_name_h, 1, 1) == "l", 0, 1)
    pos_v <- ifelse(substr(pos_name_v, 1, 1) == "t", 1, 0)
    
    return(c(pos_h = pos_h, pos_v = pos_v))
  }