
# Preparatory settings ----------------------------------------------------

## Smoothing path
set4path <- c()
set4path$sxx <- 5
set4path$row4origin <- 501
set4path$kwidth <- 75
#set4path$kwidth <- 100
set4path$xlim <- c(-50, 50)
set4path$ylim <- c(-50, 50)

## Computing curvature
library(tripack)
set4curv <- c()
set4curv$kwidth <- 150
set4curv$xlim <- c(-50, 50)
set4curv$ylim <- c(-50, 50)
set4curv$seglength <- 3
set4curv$thresh <- 35

## Create plots?
set4plot <- c()
set4plot$plot <- T



# Compute GPS path median -------------------------------------------------

## Select data section
dat2proc <- can_sxx_dist_m_rnd1.intrpl.cut.rb %>% filter(sxx == set4path$sxx)

## Compute medians
dat2proc.gps_med <-
  dat2proc %>%
  group_by(sxx, sxx_dist_m_rnd1) %>%
  summarise(gps_lon.med = median(gps_long),
            gps_lat.med = median(gps_lat)) %>%
  data.frame()



# Visualise GPS path median -----------------------------------------------

plotdat.gps_med <-
  ggplot() +
  geom_path(data = dat2proc.gps_med,
            aes(x = gps_lon.med,
                y = gps_lat.med))

if(set4plot$plot == T) plot(plotdat.gps_med)



# Smooth GPS path ---------------------------------------------------------

## Smooth GPS-variables seperate! (Reason: values can't be a real function)

## Compute model and predict values for lateral GPS values
model <-
  loess(dat2proc.gps_med$gps_lat.med ~
          c(1:length(dat2proc.gps_med$gps_lat.med)),
        span = 1/10, degree = 1)
smooth_vals_lat <- predict(model, c(1:length(dat2proc.gps_med$gps_lat.med)))

## Compute model and predict values for longitudinal GPS values
model <-
  loess(dat2proc.gps_med$gps_lon.med ~
          c(1:length(dat2proc.gps_med$gps_lon.med)),
        span = 1/10, degree = 1)
smooth_vals_lon <- predict(model, c(1:length(dat2proc.gps_med$gps_lon.med)))



# Visualise smooth gps path -----------------------------------------------

if(set4plot$plot == T)
plot(dat2proc.gps_med$gps_lon.med,
     dat2proc.gps_med$gps_lat.med,
     type = "l",
     lwd = 15)

if(set4plot$plot == T)
lines(smooth_vals_lon,
      smooth_vals_lat,
      col = "red",
      lwd = 5)



# Convert GPS data to xy distances ----------------------------------------

## Create data copy
#dat2proc.gps_med.conv <- dat2proc.gps_med
dat2proc.gps_med.conv <-
  data.frame(gps_lon.med = smooth_vals_lon,
             gps_lat.med = smooth_vals_lat)

## Determine origin
## Depending on setting (1 = real origin, 501 = critdist)
origin <-
  data.frame(
    gps_lon.med = dat2proc.gps_med.conv$gps_lon.med[set4path$row4origin],
    gps_lat.med = dat2proc.gps_med.conv$gps_lat.med[set4path$row4origin] )

## Conversion
dat2proc.gps_med.conv <-
  dat2proc.gps_med.conv %>%
  ## Compute distance from origin (critical distance) to each coordinate
  ## ... by keeping constant either lateral or longitudinal coordinate
  rowwise %>%
  #group_by(sxx_dist_m_rnd1) %>%
  mutate(gps_lon.dist = distm(origin, c(gps_lon.med, origin$gps_lat.med)),
         gps_lat.dist =
           distm(origin, c(origin$gps_lon.med, gps_lat.med))) %>%
  ## Convert to numeric data (was list before)
  mutate(gps_lon.dist = as.numeric(gps_lon.dist),
         gps_lat.dist = as.numeric(gps_lat.dist)) %>%
  ## Correct distances by taking origin into account
  mutate(gps_lon.dist =
           ifelse(gps_lon.med < origin$gps_lon.med,
                  gps_lon.dist * -1,
                  gps_lon.dist),
         gps_lat.dist =
           ifelse(gps_lat.med < origin$gps_lat.med,
                  gps_lat.dist * -1,
                  gps_lat.dist)) %>%
  # group_by(sxx) %>%
  # mutate(gps_lon.dist.cum = cumsum(gps_lon.dist),
  #        gps_lat.dist.cum = cumsum(gps_lat.dist)) %>%
  data.frame()



# Filter data (for special cases) -----------------------------------------

if(set4path$sxx == 9)
  dat2proc.gps_med.conv <-
  dat2proc.gps_med.conv %>%
  filter(gps_lat.dist > -35)



# Visualise xy-distance ---------------------------------------------------

plotdat.gps_med.conv <-
  ggplot() +
  geom_path(data = dat2proc.gps_med.conv,
            aes(x = gps_lon.dist,
                y = gps_lat.dist),
            size = 2,
            alpha = 0.25) +
  coord_cartesian(xlim = set4path$xlim,
                  ylim = set4path$ylim)

if(set4plot$plot == T)
plot(plotdat.gps_med.conv)



# Smoothing line with rolling average -------------------------------------

if(set4path$sxx == 14) {

  dat2proc.gps_med.conv.rollavg <-
    data.frame(
      gps_lon.dist.rollavg =
        rollmean(dat2proc.gps_med.conv$gps_lon.dist, set4path$kwidth),
      gps_lat.dist.rollavg =
        rollmean(dat2proc.gps_med.conv$gps_lat.dist, set4path$kwidth))

} else {

  dat2proc.gps_med.conv.rollavg <-
    data.frame(
      gps_lon.dist.rollavg =
        dat2proc.gps_med.conv$gps_lon.dist,
      gps_lat.dist.rollavg =
        dat2proc.gps_med.conv$gps_lat.dist)

}



# Visualise smoothed xy-distances -----------------------------------------

plotdat.gps_med.conv.rollavg <-
  plotdat.gps_med.conv +
  geom_path(data = dat2proc.gps_med.conv.rollavg,
            aes(x = gps_lon.dist.rollavg,
                y = gps_lat.dist.rollavg),
            colour = "red",
            size = 1) +
  coord_cartesian(xlim = set4path$xlim,
                  ylim = set4path$ylim) +
  ggtitle(paste("kwidth:", set4path$kwidth))

if(set4plot$plot == T) plot(plotdat.gps_med.conv.rollavg)




# Compute curvature -------------------------------------------------------



## Create data copy
dat4curv <- dat2proc.gps_med.conv.rollavg

## In case of duplicate values (circum won't work), create mini-mini-mini-deviations
dat4curv$gps_lon.dist.rollavg <-
  jitter(dat4curv$gps_lon.dist.rollavg, factor = 1/10^10)
dat4curv$gps_lat.dist.rollavg <-
  jitter(dat4curv$gps_lat.dist.rollavg, factor = 1/10^10)



# Compute radius ----------------------------------------------------------

## Initialise collector
coll <- c()

## For each segment of three points
for(i in 1:(nrow(dat4curv)-set4curv$seglength)) {

  dat2proc_temp <- c()

  #dat2proc_temp$x <- 1:3
  dat2proc_temp$x <- dat4curv[i:(i+set4curv$seglength-1),1]
  dat2proc_temp$y <- dat4curv[i:(i+set4curv$seglength-1),2]

  #dat2proc_temp <- dat4curv[i:(i+2),]
  #names(dat2proc_temp) <- c("x", "y")

  ## Compute radius
  #result <- circum(dat2proc_temp$x, dat2proc_temp$y)
  result <- circum(dat2proc_temp$x, dat2proc_temp$y)

  ## And collect
  coll <- c(coll, result$radius)
  cat(i, "\n")
}




# Smooth and plot radius values -------------------------------------------

## Settings for plot combination (including next step)
par(mfrow = c(4, 1))

## Plot original values
plot(coll, type = "l", main = "Computed radius values")

## Filter values
coll.filtered <- coll
#coll.filtered[coll.filtered > set4curv$thresh] <- set4curv$thresh
coll.filtered[coll.filtered > 100] <- 100

## Plot filtered values
plot(coll.filtered, type = "l", main = "Filtered radius values")

## Remember maximum value
coll.filter.max <- max(coll.filtered)

## Create smooth model
model <-
  loess(coll.filtered ~ c(1:length(coll.filtered)),
        span = 1/10,
        degree = 1)

## Predict smoothed values
coll.filtered.smoothed <- predict(model, c(1:length(coll.filtered)))

## In case of overfitting: Adjust peak values to original maximum
coll.filtered.smoothed[which(coll.filtered.smoothed > coll.filter.max)] <-
  coll.filter.max

## Plot smoothed values
plot(coll.filtered,
     type = "l",
     main = "Smoothed radius values (span = 1/10, degree = 1)")
lines(coll.filtered.smoothed, col = "red")



# Compute curvature -------------------------------------------------------

curv <- 1 / coll.filtered.smoothed

#curv <- curv^(1/3)
#curv <- curv^2

## Plot curvature values
plot(curv, type = "l", main = "Adjusted curvature ^ (1/3)")

## Reset of plot settings
par(mfrow = c(1, 1))


# Visualise curvature -----------------------------------------------------

temp <- c(head(curv, 1), curv, tail(curv, 2))

dat4plot <- cbind(dat4curv, temp)

plotcurv <-
  ggplot() +
  geom_path(data = dat4plot,
            aes(x = gps_lon.dist.rollavg,
                y = gps_lat.dist.rollavg),
            size = 2) +
  geom_path(data = dat4plot,
            aes(x = gps_lon.dist.rollavg,
                y = gps_lat.dist.rollavg,
                #alpha = curv.rollmean),
                alpha = temp),
            #alpha = test.z3),
            colour = "yellow",
            #alpha = dat2plottest$curv.adj.rollmean.z,
            size = 2,
            linemitre = 1) +
  guides(alpha = F)+
  coord_cartesian(xlim = set4curv$xlim,
                  ylim = set4curv$ylim) +
  ggtitle(paste("kwidth:", set4curv$kwidth, "+ treshold:", set4curv$thresh))

plot(plotcurv)

test <- arrangeGrob(plotdat.gps_med.conv.rollavg, plotcurv, nrow = 1)

ggsave(paste(sprintf("s%02d", set4path$sxx),
             "_seg", set4curv$seglength,
             "_xydist.smoothed.curv.pdf", sep = ""),
       test,
       path = "plots",
       dpi = 300,
       width = 20,
       height = 10,
       units = "cm")

source("analysis-study-1/160203_gps-paths/test.R")
