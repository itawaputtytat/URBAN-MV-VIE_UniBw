predLiebner_initBN_A_S_ST <- function(sett_bn) {
  
  ## Create conditional probability table for observation node
  node_I <- cptable(~ I, values = sett_bn$priors$I, levels = sett_bn$state_names$I)
  node_ST <- cptable(~ ST, values = sett_bn$priors$ST, levels = sett_bn$state_names$ST)
  node_S <- cptable(~ S|ST, values = sett_bn$priors$S, levels = sett_bn$state_names$S$S)
  node_A <- cptable(~ A|S:ST, values = sett_bn$priors$A_S, levels = sett_bn$state_names$A$A)
  node_O <- cptable(~ O|S:A:I, values = sett_bn$priors$O, levels = sett_bn$state_names$O)
  
  # ## Compile conditional probability tables
  cpt_compiled <- compileCPT(list(node_I, node_S, node_A, node_O, node_ST))
  
  ## Build graphical independent network
  network <- grain(cpt_compiled)
  
  return(network)
}
