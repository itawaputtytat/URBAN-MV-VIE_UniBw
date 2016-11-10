
# Preparatory settings ----------------------------------------------------

## General file path
filepath <- file.path(
  "E:",
  "URBAN-MV-VIE_UniBw",
  "Sekundärdaten",
  "Blaschke",
  "Studentische-Arbeiten",
  "Weinhold_2006_Daten",
  "Daten",
  "Messdaten aufbereitet"
)



# Processing regular data -------------------------------------------------

list4dir <- list.dirs(filepath, recursive = F)
list4dir <- list4dir[which(grepl("VP", list4dir))]

for(d in list4dir) {
  filepath_temp <- d
  subid <- substr(filepath_temp, nchar(filepath) + 4, nchar(filepath_temp))
  subid <- as.numeric(subid)
  
  filelist <- list.files(filepath_temp)
  names4files <- filelist
  names4files <- 
    sapply(names4files, function(f) {
      f <- gsub(".Versuch", "versuch", f)
      f <- gsub(". Versuch", "versuch", f)
      f <- gsub("1.versuch", "_versuch1", f)
      f <- gsub("2.versuch", "_versuch2", f)
      f <- gsub("3.versuch", "_versuch3", f)
      f <- gsub("1versuch", "_versuch1", f)
      f <- gsub("2versuch", "_versuch2", f)
      f <- gsub("3versuch", "_versuch3", f)
      f <- gsub("falscher Filename", "_falscherfilname", f)
      f <- gsub("flieg", "_flieg", f)
      f <- gsub("folge", "_folge", f)
      f <- gsub(" folg", "_folge", f)
      f <- gsub("\\(evtl defekt\\)", "_evtldefekt", f)
      f <- gsub(" Nr.", "_nr", f)
      f <- gsub("\\. ", "", f)
      f <- gsub(" ", "", f)
      f <- gsub("Ü", "ue", f)
      f <- gsub(".csv", "", f)
      f <- tolower(f)
      })
  
  names4files <- as.vector(names4files)
  names4files <- paste(sprintf("%02d", subid), names4files, sep = "_")

  for(f in 1:length(filelist)) {

    ## Load and process data
    dat2proc <- read.csv2(file.path(filepath_temp, filelist[f]), sep = ",")
    dat2proc <- rbind(dat2proc, 0)
    dat2proc <- apply(dat2proc, 2, function(x) {
      x <- as.numeric(x)
      x <- na.locf(x, na.rm = F)
      x <- na.locf(x, na.rm = F, fromLast = T)
    }) %>% as.data.frame()
    dat2proc <- dat2proc[-nrow(dat2proc), ]

    ## Write to database
    name4t <- paste("t_", names4files[f], sep = "")
    outputString(paste("* Write to database:", name4t))
    dbWriteTable(dbconn_secdata_blaschke, name4t, dat2proc, row.names = F, overwrite = T)
  }
}

# testdat <- dbGetSrc("dbconn_secdata_blaschke", "t_01_abb100")
# plot(na.locf(as.numeric(testdat$Geschwindigkeit_15Bit)), type = "l")
# 
# 
# teststr <- "Abb. 80 2. Versuch.csv"
# teststr <- gsub(" ", "_", teststr)
# teststr
# teststr <- gsub("._", "", teststr)
# teststr
