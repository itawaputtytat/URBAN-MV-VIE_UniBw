
# Objective ---------------------------------------------------------------

## Use available algorithms for clustering


# Function ----------------------------------------------------------------

clust2groups <-
  function (dat2proc.wide,
            k = 3,
            procedure = "kmeans",
            algorithm = "Hartigan-Wong",
            iter.max = 500,
            nstart = 1,
            start = "random",
            measure4diss = "EUCL",
            method4agglo = "single",
            seed = 42,
            ...) {

    cat("\n")
    cat("Running clust2groups: \n")
    cat("--------------------  \n")
    cat("\n")

    cat("* Using function", procedure, "...", "\n")


    set.seed(seed)


    ## kmeans ----
    if (procedure == "kmeans")
      cluster_output <-
        clust2groups_kmeans(dat2proc.wide, k,
                            iter.max, nstart, algorithm, ...)


    ## kmeanspp ----
    if (procedure == "kmeanspp")
      cluster_output <-
        clust2groups_kmeanspp(dat2proc.wide, k, start,
                              iter.max, nstart, algorithm, ...)


    # hclust ----
    if (procedure == "hclust")
      cluster_output <-
        clust2groups_hclust(dat2proc.wide, measure4diss, method4agglo, k, ...)

    ## No "Done!" needes, as outputs comes from other functions

    return (cluster_output)

}
