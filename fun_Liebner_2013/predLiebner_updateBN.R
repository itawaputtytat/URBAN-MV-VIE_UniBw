predLiebner_updateBN <- function(name_bn, 
                                 likelihood_O, 
                                 state_names_O) {
  
  ## Get BN
  bn <- get(name_bn)
  
  ## Get CPT for each node
  cpt <- bn[["cptlist"]]
  ## Create conditional probability table for observation node
  cpt$O <- cptable(~ O|I:A:S, values = likelihood_O, levels = state_names_O)
  
  ## Compile conditional probability tables
  cpt_compiled <- compileCPT(cpt)
  
  ## Build graphical independent network
  bn <- grain(cpt_compiled)
}