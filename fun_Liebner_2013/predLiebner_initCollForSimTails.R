predLiebner_initCollForSimTails <- function(n_per_element) {
  
  ## Calculate number of hypotheses
  n_hypotheses <- prod(n_per_element)
  
  ## Create list with components for each hypothesis
  hypotheses_list <- vector("list", n_hypotheses)
  
  ## Rename list components
  names(hypotheses_list) <- names(idm_createSimDat(n_per_element))
  
  return(hypotheses_list)
}