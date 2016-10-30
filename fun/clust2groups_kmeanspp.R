
# Output for initialising -------------------------------------------------

cat("* Function initialised: clust2groups_kmeanspp \n")



# Function ----------------------------------------------------------------

clust2groups_kmeanspp <-
  function (dat2proc.wide,
            k = 3,
            start = "random",
            iter.max = 500,
            nstart = 1,
            algorithm = "Hartigan-Wong",
            ...) {

    cat("\n")
    cat("Running clust2groups_kmeanspp: \n")
    cat("------------------------------ \n")

    cat("* Using dataframe: ", deparse(substitute(dat2proc.wide)), "\n", sep = "")

    cat("* kmeans-clustering using ... \n")
    cat("** centers:", k, "\n")
    cat("** start:", start, "\n")
    cat("** iter.max:", iter.max, "\n")
    cat("** nstart:", nstart, "\n")
    cat("** algorithm:", algorithm, "\n")
    cat("\n")

    ## Run algorithm
    cat("* Running kmeanspp algorithm ... \n")
    result <-
      kmeanspp(dat2proc.wide,
               k = k,
               start = start,
               iter.max = iter.max,
               nstart = nstart,
               algorithm = algorithm)


    ## Create dataframe for identifier (e.g. passing)
    ## ... and corresponding cluster-nr found using algorithm
    assignment <-
      data.frame(passing = rownames(dat2proc.wide),
                 clustgroup = factor(result$cluster),
                 row.names  = NULL)

    return (list(result = result,
                 assignment = assignment))

}
