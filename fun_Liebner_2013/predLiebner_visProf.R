predLiebner_visProf <- function(plotdat_prev, set4sim, set4dat, dat4sim, showplot) {
  
  plotdat <- plotdat_prev
  
  invisible(lapply(seq_along(dat4sim), function(x) {
    name <- names(dat4sim)[[x]]
    #print(name)
    if (grepl("j1", name)) col <- "cyan"
    if (grepl("j2", name)) col <- "orange"
    if (grepl("j3", name)) col <- "red"
    if (grepl("j4", name)) col <- "magenta"
    plotdat <<-
      plotdat +
      geom_line(data = dat4sim[[x]],
                # aes_string(x = paste(paste(set4dat$varname4dist_m, "sim", "j", sep = "_"), j,
                #                      "_k", k,
                #                      "_l", l ,
                #                      sep = ""),
                #            y = paste(paste(set4dat$varname4speed, "sim", "j", sep = "_"), j,
                #                      "_k", k,
                #                      "_l", l,
                #                      sep = "")),
                aes(x = dist_m,
                    y = speed_ms),
                col = col)
  }))
  # for (j in 1:length(set4sim$computeI)) {
  #   if(set4sim$computeI[j] == T) {
  #     for (k in 1:length(set4sim$v_ms.max)) {
  #       for (l in 1:length(set4sim$acclon_ms2.max)) {
  #         
  #         if(j == 1) color4j <- "cyan"
  #         if(j == 2) color4j <- "orange"
  #         if(j == 3) color4j <- "red"
  #         if(j == 4) color4j <- "magenta"
  #         
  #         if(k == 1) color4j <- paste(color4j, "1", sep = "")
  #         if(k == 2) color4j <- paste(color4j, "1", sep = "")
  #         if(k == 3) color4j <- paste(color4j, "1", sep = "")
  #         
  #         plotdat <-
  #           plotdat +
  #           geom_line(data = dat4sim,
  #                     # aes_string(x = paste(paste(set4dat$varname4dist_m, "sim", "j", sep = "_"), j,
  #                     #                      "_k", k,
  #                     #                      "_l", l ,
  #                     #                      sep = ""),
  #                     #            y = paste(paste(set4dat$varname4speed, "sim", "j", sep = "_"), j,
  #                     #                      "_k", k,
  #                     #                      "_l", l,
  #                     #                      sep = "")),
  #                     aes(x = dist_m,
  #                         y = speed_ms), 
  #                     color = color4j) +
  #         coord_cartesian(xlim = c(-50, 25))
  #         
  #       } # set4sim$v_ms.max
  #     } # set4sim$acclon_ms2.max
  #   } else {}
  # } # set4sim$computeI
  
  if (showplot) plot(plotdat)
  return(plotdat)
}


# fsdfsd ------------------------------------------------------------------



# plotdat <- plotdat_passing.dvm.pos
# invisible(lapply(seq_along(dat4sim), function(x) {
#   name <- names(dat4sim)[[x]]
#   print(name)
#   if (grepl("j1", name)) col <- "cyan" 
#   if (grepl("j2", name)) col <- "orange" 
#   if (grepl("j3", name)) col <- "red" 
#   if (grepl("j4", name)) col <- "magenta"
#   plotdat <<-
#     plotdat +
#     geom_line(data = dat4sim[[x]],
#               # aes_string(x = paste(paste(set4dat$varname4dist_m, "sim", "j", sep = "_"), j,
#               #                      "_k", k,
#               #                      "_l", l ,
#               #                      sep = ""),
#               #            y = paste(paste(set4dat$varname4speed, "sim", "j", sep = "_"), j,
#               #                      "_k", k,
#               #                      "_l", l,
#               #                      sep = "")),
#               aes(x = dist_m,
#                   y = speed_ms),
#               col = col)
# }))