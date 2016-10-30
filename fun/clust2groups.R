
# Objective ---------------------------------------------------------------

## Use available algorithms for clustering



# Output for initialising -------------------------------------------------

cat("* Function initialised: clust2groups \n")



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
      clustoutput <-
        clust2groups_kmeans(dat2proc.wide, k,
                            iter.max, nstart, algorithm, ...)


    ## kmeanspp ----
    if (procedure == "kmeanspp")
      clustoutput <-
        clust2groups_kmeanspp(dat2proc.wide, k, start,
                              iter.max, nstart, algorithm, ...)


    # hclust ----
    if (procedure == "hclust")
      clustoutput <-
        clust2groups_hclust(dat2proc.wide, measure4diss, method4agglo, k, ...)

    ## No "Done!" needes, as outputs comes from other functions

    return (clustoutput)

}









#
#
# #join data and cluster-nr
# data_clust_kmeanspp <-
#   left_join(data2clust,
#             clust_kmeanspp_groups,
#             by = "passing")
#
# data_clust_kmeanspp$clust_nr <- factor(data_clust_kmeanspp$clust_nr)
#
# #plot data
# data_plot <-
#   ggplot(data_clust_kmeanspp) +
#   geom_line(aes_string(x = "temp_dist",
#                        y = "temp_var2clust",
#                        group = "passing",
#                        colour = "clust_nr")) +
#   stat_summary(aes_string(x = "temp_dist",
#                           y = "temp_var2clust"),
#                geom = "line",
#                fun.y = "mean",
#                colour = "black",
#                size = 1) +
#   ggtitle(paste("kmeanspp", set4clust$k, "(", set4clust$var2clust , ")", sep = "")) +
#   coord_cartesian(ylim = c(0, 17))
# #dev.new(); data_plot
# dev.new(); data_plot + facet_grid(.~clust_nr)
#
# #plotting cluster solutions
#
# dev.new()
# clusplot(data2clust_wide, clust_kmeanspp$cluster,
#          colour = T,
#          shade = T,
#          labels = 2,
#          cex = 0.75,
#          lines = 0)
#
# library(fpc)
# #dev.new(); plotcluster(data2clust_wide, clust_kmeanspp$cluster)
#
# #validating cluster solutions
# data_dist_eucl <- dist(data2clust_wide, method = "euclidean")
# cluster.stats(data_dist_eucl, clust_kmeanspp$cluster)

#rm(clust_kmeans, clust_kmeanspp_groups, data_clust_kmeanspp, data_plot)
