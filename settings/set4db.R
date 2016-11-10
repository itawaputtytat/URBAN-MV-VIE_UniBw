set4db <- c()
set4db$dns  <- "URBAN-MV-VIE_UniBw_"
set4db$host <- "localhost"
set4db$port <- 5432
set4db$name <- set4db$dns
set4db$user <- "postgres"
set4db$pwd  <- "WRITE-PASSWORD-HERE"
set4db$drv  <- dbDriver("PostgreSQL")
set4db$select <- c("Study1", "Study2", "SecData_Blaschke")
set4db$prompt <- 
  paste(paste("[", 1:length(set4db$select), "]", sep = ""), set4db$select)
set4db$input <- c()
