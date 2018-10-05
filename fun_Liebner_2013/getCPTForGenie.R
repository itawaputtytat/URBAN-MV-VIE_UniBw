getCPTForGenie <- function(bn, node_name) {

  ## Get CPT list
  cpt <- bn$cptlist[[node_name]]
  
  ## Get dimensions
  ## Get dimension names
  cpt_dim <- dim(cpt)
  node_names <- names(cpt_dim)
  #print(node_names)
  
  ## Extract values
  cpt_df <- plyr::adply(cpt, 1:length(cpt_dim))
  
  ## Rename last column
  names(cpt_df) <- c(names(cpt_df)[1:(ncol(cpt_df)-1)], "V1")

  ## Look for related nodes
  ## Remove selected node from list of related nodes
  node_names_arrange2 <- !node_names %in% node_name
  node_names_arrange2 <- node_names[node_names_arrange2]
  
  ## If there are no related nodes, only add node name
  if (length(node_names_arrange2) >= 1) {
    node_names_arrange <-c(node_name, node_names_arrange2)
  } else {
    node_names_arrange2 <- node_name
  }
  
  ## Sort values
  if (node_name == "O") {
    cpt_df <- cpt_df %>% arrange_(.dots = c(node_name, c("S", "A", "I")))
    print(cpt_df)
  } else {
    cpt_df <- cpt_df %>% arrange_(.dots = c(node_names_arrange2))
  }
  
  ## Reorder columns
  if (exists("node_names_arrange")) {
    
    cpt_df_unite <- 
      cpt_df %>%
      unite_("unite_col", node_names_arrange2)
    
    ## Convert to wide format
    cpt_df_spread <- 
      cpt_df_unite %>% 
      spread(unite_col, V1)
  
    names <- unique(cpt_df_unite$unite_col)
      
    cpt_df_spread <-
      cpt_df_spread[, c(node_name, names)]
      #cpt_df_spread[node_names_arrange]
  } else {
    cpt_df_spread <- cpt_df#cpt_df_spread[,names]
  }

  write.table(cpt_df_spread[,-1], 
              "clipboard", 
              row.names = FALSE, 
              col.names = FALSE,
              sep = "\t")
  
  return(cpt_df_spread)
}
