

# Preparatory settings ----------------------------------------------------

sett_dat <- c()
sett_dat$col_name$case <- "passing"
sett_dat$col_name$subject <- "subject_id"
sett_dat$col_name$time <- "time_s"

sett_string <- c()
sett_string$start <- "Start \"\""
sett_string$dir_vlc <- 
  paste("C:", "Program Files", "VideoLAN", "VLC", "vlc.exe", sep = "\\")
sett_string$dir_vlc <- paste0("\"", sett_string$dir_vlc, "\"")
sett_string$dir_video <- 
  paste("E:", "URBAN-MV-VIE_UniBw", "Studie1_Realverkehr", "04_Daten", 
        "Videos_Front", "", sep = "\\")
sett_string$dir_video <- paste0("\"", sett_string$dir_video)
sett_string$vlc_params_loop = "--loop"
sett_string$time_start = "--start-time="
sett_string$time_stop = "--stop-time="



# Get filename information ------------------------------------------------

dat_filenames <- 
  dbGetSrc(dbFindConnObj("Study-1"), 
           "t_filenames_adtf_prefix")




# Create batch ------------------------------------------------------------

for (case in dat[, sett_dat$col_name$case]) {
  
  outputString(paste("* Processing case:", case))
  
  ## Extract time
  row_finder_case <- dat[, sett_dat$col_name$case] == case
  t1 <- dat[row_finder_case, paste_(sett_dat$col_name$time, "min")] 
  t2 <- dat[row_finder_case, paste_(sett_dat$col_name$time, "max")] 
  
  ## Extract filename
  row_finder_file <- 
    which(dat_filenames[, sett_dat$col_name$subject] == 
            dat[row_finder_case, sett_dat$col_name$subject])
  
  filename <- dat_filenames[row_finder_file, "filename_prefix"]
  filename <- paste0(filename, "_Front.avi")
  
  ## Stop if subject_id is 3 or 19
  subject_id <- dat_filenames[row_finder_file, sett_dat$col_name$subject]
  if (subject_id %in% c(3, 19)) {
    next
  }
  
  filename <- paste_(sprintf("Vp%02d", subject_id), filename)
  
  batch_string <- c()
  batch_string <- 
    paste(sett_string$start,
          sett_string$dir_vlc,
          sett_string$dir_video, 
          collapse = " ")
  
  batch_string <- paste0(batch_string, filename, "\"")
  
  batch_string <- paste(batch_string, 
                        sett_string$vlc_params_loop,
                        paste0(sett_string$time_start, t1),
                        paste0(sett_string$time_stop, t2 ))

  #cat(paste(batch_string, collapse = " "))

  ## Create batch
  ## Use case[1] as workaround, to avoid error due to vector format
  write(batch_string, paste0(case, ".bat"))
  
}

