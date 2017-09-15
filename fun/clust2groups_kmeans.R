clust2groups_kmeans <-
  function (dat2proc.wide,
            k = 3,
            iter.max = 500,
            nstart = 1,
            algorithm = "Hartigan-Wong",
            ... ) {

    cat("\n")
    cat("Running clust2groups_kmeans: \n")
    cat("---------------------------- \n")

    cat("* Using dataframe: ", deparse(substitute(dat2proc.wide)), "\n", sep = "")

    cat("* kmeans-clustering using ... \n")
    cat("** centers:", k, "\n")
    cat("** iter.max:", iter.max, "\n")
    cat("** nstart:", nstart, "\n")
    cat("** algorithm:", algorithm, "\n")
    cat("\n")

    ## Run algorithm ----
    cat("* Running kmeans algorithm ... \n")
    output <-
      kmeans(dat2proc.wide,
             centers   = k,
             iter.max  = iter.max,
             nstart    = nstart,
             algorithm = algorithm)

    cat("\n")
    cat("Done! \n")
    cat("\n\n")

    ## Create dataframe for identifier (e.g. passing)
    ## ... and corresponding cluster-nr found using algorithm
    assignment <-
      data.frame(passing = rownames(dat2proc.wide),
                 cluster_group = output$cluster,
                 row.names  = NULL)

    return (list(result = output,
                 assignment = assignment))

}
