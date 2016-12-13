predLiebner_updateBN <- function(name4bn, O, set4bn) {
  
  ## Create conditional probability table for observation node
  node_O <- cptable(~ O|I:V:A, values = O, levels = set4bn$states$O)
  
  node_I <- bn[["cptlist"]]$I
  node_V <- bn[["cptlist"]]$V
  node_A <- bn[["cptlist"]]$A
  
  ## Compile conditional probability tables
  cpt_compiled <- compileCPT(list(node_I, node_V, node_A, node_O))
  
  ## Build graphical independent network
  network <- grain(cpt_compiled)
  #plot(network)
  #iplot(network)
  
}