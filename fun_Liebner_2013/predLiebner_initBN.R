predLiebner_initBN <- function(version, set4bn) {
  
  if (version == "V1") {
    ## Coerce indizes into list (with LETTER as node indicator)
    ## Reason: Appropriate names for probability tables
    set4bn$states$I_list <- list(I = set4bn$states$I)
    set4bn$states$V_list <- list(V = set4bn$states$V)
    set4bn$states$V_I_list <- c(set4bn$states$V_list, set4bn$states$I_list)
    set4bn$states$A_list <- list(A = set4bn$states$A)
    set4bn$states$O_list <- list(O = set4bn$states$O)
    set4bn$states$A_V_list <- c(set4bn$states$A_list, set4bn$states$V_list)
    
    ## Settings for nodes
    set4bn$prior$I <-
      array(set4bn$prior$I,
            dim = length(set4bn$states$I),
            dimnames = set4bn$states$I_list)
    
    ## Assuming same probability for maximum u models as for DVM
    set4bn$prior$V <-
      array(set4bn$prior$V,
            dim = length(set4bn$states$V),
            dimnames = set4bn$states$V_list)
    
    ## Assuming uniform probability for maximum u models compared to DVM
    # set4bn$prior$V <-
    #   array(unlist(set4bn$prior$V_I),
    #         dim = c(length(set4bn$states$V), length(set4bn$states$I)),
    #         dimnames = set4bn$states$V_I_list)
    
    set4bn$prior$A_V <-
      array(unlist(set4bn$prior$A_V), ## Must be unlist, as it is already 2-dim
            dim = c(length(set4bn$states$A), length(set4bn$states$V)),
            dimnames = set4bn$states$A_V_list)
    
    ## Create conditional probability table for observation node
    node_I <- cptable(~ I, values = set4bn$prior$I, levels = set4bn$states$I)
    node_V <- cptable(~ V, values = set4bn$prior$V, levels = set4bn$states$V)
    #node_V <- cptable(~ V|I, values = set4bn$prior$V, levels = set4bn$states$V)
    node_A <- cptable(~ A|V, values = set4bn$prior$A_V, levels = set4bn$states$A)
    ## See settings/set4bn.R for set4bn$prior$O
    node_O <- cptable(~ O|I:V:A, values = set4bn$prior$O, levels = set4bn$states$O)
    
    # ## Compile conditional probability tables
    cpt_compiled <- compileCPT(list(node_I, node_V, node_A, node_O))
    
    ## Build graphical independent network
    network <- grain(cpt_compiled)

    return(network)
  }
}

