findColNameForAM <- function(am, dat) {
  names <- names(dat)
  res <- grep(am, names, value = T)
  return (res)
}