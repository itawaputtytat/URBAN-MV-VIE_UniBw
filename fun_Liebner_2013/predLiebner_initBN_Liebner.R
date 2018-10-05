predLiebner_initBN_Liebner <- function(sett_bn) {
  
  ## Create conditional probability table for observation node
  node_I <- cptable(~ I, values = sett_bn$priors$I, levels = sett_bn$state_names$I)
  node_S <- cptable(~ S, values = sett_bn$priors$S, levels = sett_bn$state_names$S)
  node_A <- cptable(~ A|S, values = sett_bn$priors$A_S, levels = sett_bn$state_names$A)
  node_O <- cptable(~ O|I:A:S, values = sett_bn$priors$O, levels = sett_bn$state_names$O)
  
  # ## Compile conditional probability tables
  cpt_compiled <- compileCPT(list(node_I, node_S, node_A, node_O))
  
  ## Build graphical independent network
  network <- grain(cpt_compiled)
  
  return(network)
}

