clust2groups_hclust <-
  function (dat2proc.wide,
            method4dist = "EUCL",
            method4agglo = "single",
            k = 3,
            ...) {

    cat("\n")
    cat("Running clust2groups_hclust: \n")
    cat("---------------------------- \n")

    cat("* Using dataframe: ", deparse(substitute(dat2proc.wide)), "\n", sep = "")

    cat("* hclust-clustering using ... \n")
    cat("** dissimilartiy measure:", method4dist, "\n")
    cat("** agglomeration method:", method4agglo, "\n")
    cat("** cutree:", k, "\n")

    cat("\n")


    ## Compute distance matrix using package TSclust
    #data2clust_wide_t <- t(dat2proc.wide)
    cat("* Computing distance matrix ... \n")
    distmat <- diss(as.matrix(dat2proc.wide), method4dist)

    cat("\n")
    cat("Done! \n")
    cat("\n")


    # Run algorithm
    cat("* Running hclust algorithm ... \n")
    output <- hclust(distmat, method4agglo)

    cat("\n")
    cat("Done! \n")
    cat("\n\n")

    #new table for subids and corresponding cluster-nr
    assignment <-
      data.frame(passing = rownames(dat2proc.wide),
                 cluster_group   = cutree(output, k = k),
                 row.names  = NULL)

  return (list(output = output,
               assignment = assignment))

}
