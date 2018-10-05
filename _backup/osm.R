#install.packages("osmar")


library(devtools)
assignInNamespace("version_info", c(devtools:::version_info, list("3.5" = list(version_min = "3.3.0", version_max = "99.99.99", path = "bin"))), "devtools")
find_rtools() # is TRUE now

devtools::install_github("ropensci/osmdata")

library(osmar)

box <- 
  center_bbox(
    center_lon = 11.650273,
    center_lat = 48.077738,
    width = 20,
    height = 100
  )
src <- osmsource_api(url = "https://api.openstreetmap.org/api/0.6/")
class(box) <- "bbox"
dat <- get_osm(box, source = src)
plot(dat)
ways <- find(dat, way(tags(k == "highway")))
ways <- find_down(dat, way(ways))
ways <- subset(dat, ids = ways)

# SpatialLinesDataFrame object
hw_lines <- as_sp(ways, "lines")  
spplot(hw_lines, zcol = "uid")
gpsPoints <- spsample(x = hw_lines, n = 100, type = "random")

plot(hw_lines, xlab = "Lon", ylab = "Lat")
plot(gpsPoints, add = TRUE, pch = 19, col = "red")
box()
distances <- dist2Line(p = gpsPoints, line = hw_lines)


test <- unclass(hw_lines)
test2 <- attr(test, "lines")

plot(test2[[1]]@Lines[[1]]@coords, type = "l", 
     xlim = c(11.65, 11.651),
     ylim = c(48.0760, 48.0790))
lines(test2[[1]]@Lines[[1]]@coords, type = "l")

        