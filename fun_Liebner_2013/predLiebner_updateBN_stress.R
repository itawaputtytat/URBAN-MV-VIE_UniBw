predLiebner_updateBN_stress <- function(name_bn, 
                                        likelihood_O, 
                                        state_names_O) {
  
  ## Get BN
  bn <- get(name_bn)
  
  ## Create conditional probability table for observation node
  node_O <- 
    cptable(~ O|I:S:A, 
            values = likelihood_O, 
            levels = state_names_O)
  
  node_I <- bn[["cptlist"]]$I
  node_S <- bn[["cptlist"]]$S
  node_A <- bn[["cptlist"]]$A
  node_stress <- bn[["cptlist"]]$stress
  
  ## Compile conditional probability tables
  cpt_compiled <- compileCPT(list(node_I, node_S, node_A, node_O, node_stress))
  
  ## Build graphical independent network
  network <- grain(cpt_compiled)
}