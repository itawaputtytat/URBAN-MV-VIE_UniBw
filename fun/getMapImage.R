getMapImage <- function(sxx, set4map = NULL, zoom = 18) {
  filepath <- file.path("resources/study1/map-images")
  filelist <- list.files(filepath)
  filelist <- filelist[grepl("RData", filelist)]
  filelist <- filelist[grepl(sprintf("s%02d", sxx), filelist)]
  filelist <- filelist[grepl(paste("zoom", zoom, sep = ""), filelist)]
  filename <- filelist
  load(file.path(filepath, filename))
  return(map)
}