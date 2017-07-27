## @knitr start_markdown

library(puttytat4R)

filepath <- "init-framework"

filenames <- c(
  "db-lib.R",
  "db-init-settings.R",
  "db-fun.R",
  "data-manipulation-lib.R",
  "data-manipulation-fun.R",
  "vis-lib.R",
  "vis-fun.R",
  "data-processing-lib.R",
  #"Liebner-lib.R",
  #"Liebner-fun.R",
  "misc-settings.R"
  #"preloading-data.R"
)


lapply(filenames, function(x) source(file.path(filepath, x)))
set_db$pwd <- readLines("settings/db-pwd.txt", warn = F)