createSeqObj <- function(dat, 
                                    key_colname,
                                    value_colname,
                                    group_colname = NULL,
                                    states,
                                    labels,
                                    id_colname,
                                    order = T,
                                    start = "beg",
                                    sort.index = T,
                                    xtstep = 3,
                                    ...) {
  
  outputFunProc(R)
  
  outputString("* Create sequence object:")
  
  ## In case of no group variable: Create dummy
  if ( is.null(group_colname) ) {
    dat$group_dummy = 1
    group_colname = "group_dummy"
  }
  
  ## Initialise lists to collect values by groups
  dat_wide <- c()
  dat_seq <- c()
  id_ordered <- c()
  dat_ordered <- c()
  
  for ( g in unique(dat[, group_colname]) ) {
    
    if (group_colname != "group_dummy") {
      outputString(paste0("** Currently processing group: ", group_colname))
    }

    outputString(
      paste0("** Create wide data format (key: ", key_colname, ") ... "), 
      linebreak = F)
    
    ## Filter data by group
    dat_temp <- 
      dat %>%
      ## Filter must be done using shQuote
      ## See answer of StatCC in 
      ## https://stackoverflow.com/questions/27197617/filter-data-frame-by-character-column-name-in-dplyr
      filter_(paste(group_colname, "==", shQuote(g)))
    
    ## Spread rows to columns
    dat_wide_temp <-
      dat_temp %>% 
      select_(id_colname, key_colname, value_colname) %>% 
      spread_(key_col = key_colname, value_col = value_colname)
    
    ## Save to list
    dat_wide[[g]] <- dat_wide_temp
    
    outputDone(T)
    
    
    outputString("** Create state sequence object:")
    
    ## Create id variable
    id <- unique(dat_wide_temp[, id_colname])
    
    ## Create sequence object
    dat_seq_temp  <-
      seqdef(dat_wide_temp,
             2:ncol(dat_wide_temp),
             states = states,
             labels = labels,
             id = id,
             xtstep = xtstep)  
    
    dat_seq[[g]] <- dat_seq_temp
    
    
    ## Order sequence
    if (order) {
      
      outputString(
        paste0("** Order sequences by states ... "), 
        linebreak = F)
      
      temp_order <- sorti(dat_seq_temp, start = "beg", sort.index = T)
      temp_order <- rev(temp_order)
      id_ordered_temp <- 
        data.frame(key =  id[ temp_order ],
                   order = 1:length(temp_order))
      
      names(id_ordered_temp)[1] <- id_colname
      names(id_ordered_temp)[2] <- paste0("order_", group_colname)
      
      ## Create column with numeric order
      ## (must be done before coercing to factor, see below)
      dat_temp <- left_join(dat_temp, id_ordered_temp)
      
      dat_temp[, id_colname] <- 
        factor(dat_temp[, id_colname], levels = id_ordered_temp[, id_colname])
      
      dat_ordered <- rbind(dat_ordered, dat_temp)
                           #stringsAsFactors = F)
      
      id_ordered[[g]] <- id_ordered_temp
      
      outputDone(T)
      
    }
  }
  
  outputDone(T)
  
  return(list(dat_wide = dat_wide,
              dat_seq = dat_seq,
              id_ordered = id_ordered,
              dat_ordered = dat_ordered))
}