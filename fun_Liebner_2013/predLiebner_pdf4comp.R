predLiebner_pdf4comp <- function (a, a.est, a.sd) {
  y = 1 / ( sqrt(2* pi) * a.sd ) * exp( -1/2 * ( (a - a.est)/a.sd )^2 )
  return(y)
}
