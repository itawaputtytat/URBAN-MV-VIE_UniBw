smoothWithLoess <- function(val2proc, span = 1/10, degree = 1, onlyfitted = F) {
  model <- 
    loess(val2proc ~ c(1:length(val2proc)), 
          span = span, 
          degree = degree, 
          ## In case of NA 
          ## (see http://stackoverflow.com/questions/27796368/r-loess-prediction-returns-na)
          ## also: na.action = na.exclude
          control=loess.control(surface="direct"))
  val2proc <- predict(model, c(1:length(val2proc)))
  if (onlyfitted)
    return(val2proc) else
      return(model)
  }