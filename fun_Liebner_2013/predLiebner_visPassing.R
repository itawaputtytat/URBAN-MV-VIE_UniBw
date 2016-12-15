predLiebner_visPassing <- function(dat4plot, set4dat, showplot) {
  plotdat <-
    ggplot() +
    geom_line(data = dat4plot,
              aes_string(x = set4dat$varname4dist_m,
                         y = set4dat$varname4speed),
              size = 1.25,
              col = "#3953A4") +
    coord_cartesian(ylim = c(0, 30)) + 
    scale_x_continuous(expand = c(0, 0))
  if (showplot) plot(plotdat)
  return(plotdat)
}