## EXPLORE


# Objective ---------------------------------------------------------------

## Find the mean position of positions where steer_angle_deg is max



# Preparatory settings ----------------------------------------------------

## Initialise setting variable
set4expl <- c()

## Select situation
set4expl$sit <- 18

## Set threshold for distance for finding maximum steering angle
set4expl$dist_min <- -15
## Special case for situation 3 see below! (next section)

set4expl$treshold_factor <- 1

## Should data be plotted? (for better understanding whats going on)
set4expl$plot <- T



# Prepare data ------------------------------------------------------------

## Load data for selected situation (must be interpolated dataframes)
datname2proc <- study1_t_adtf_pxx_full_dti_rnd1_intrpld_cut


## Get data
dat2proc <- get(datname2proc)

dat2proc <- dat2proc %>% filter(pxx_dti_m_rnd1 > -20)

## (For testing purposes)
#dat2proc <- backup_intrpl

#dat2proc <- dat2proc %>% filter(subject_id == 27)

#dat2proc <- dat2proc %>% filter(subject_id > 20 & subject_id <= 25)
#dat2proc <- dat2proc %>% filter(round_txt == "stress")


## Load situation attributes for correct direction identification
## ... so that only peaks in turning direction are used for position finding
assign("t_pxx_intersection_attributes", dbGetSrc(db_conn_6, "t_pxx_intersection_attributes"), env = .GlobalEnv)


## Get direction of interest (by situation variable)
dir2expl <-
  t_pxx_intersection_attributes$direction[which(t_pxx_intersection_attributes$position_id == set4expl$sit)]


## Special case for situation 3
## Can be ignored, because it's not classified as an intersection anymore
if (set4expl$sit == 3)
  set4expl$dist_max <- 25 else
    set4expl$dist_max <- max(dat2proc$pxx_dti_m_rnd1)



# Explore data ------------------------------------------------------------

## Plot steer_angle_deg for each situation
plotdat.original <-
  ggplot() +
  geom_line(data = dat2proc %>% filter(round_txt == "intro"),
            aes(x = pxx_dti_m_rnd1,
                y = steer_angle_deg,
                group = subject_id),
            col = "blue") +
  geom_line(data = dat2proc %>% filter(round_txt == "normal"),
            aes(x = pxx_dti_m_rnd1,
                y = steer_angle_deg,
                group = subject_id),
            col = "green3") +
  geom_line(data = dat2proc %>% filter(round_txt == "stress"),
            aes(x = pxx_dti_m_rnd1,
                y = steer_angle_deg,
                group = subject_id),
            col = "red") +
  coord_cartesian(xlim = c(min(dat2proc$pxx_dti_m_rnd1),
                           max(dat2proc$pxx_dti_m_rnd1)),
                   ylim = c(-600, 600)) #+
  # scale_x_continuous(limits = c(min(dat2proc$pxx_dti_m_rnd1),
  #                             )))
  
  
  if (set4expl$plot == T) {
    
    cat("* Visualisation of original values ... \n")
    
    plotdat <-
      plotdat.original +
      ggtitle(paste("Intersection:", set4expl$sit, " - Original values"))
    
    plot(plotdat)
    
    ## For later plotting while processing use:
    plotdat.proc <- plotdat.original
    
    pauseAndContinue()
    
  }




# Start of finding position data ------------------------------------------

# Preparation -------------------------------------------------------------

## Subset data for current direction (avoiding peaks in wrong direction)
## ... and therefore backup data for later re-plotting
dat2proc_backup <- dat2proc


## If direction == left, only positive values will be identified
## otherwise negative values will be used for finding maximum of steering value
if (dir2expl == "left")
  dat2proc <- dat2proc %>% filter(steer_angle_deg > 0) else
    dat2proc <- dat2proc %>% filter(steer_angle_deg < 0)



# Finding process ---------------------------------------------------------

cat("* Find position where abs(steer_angle_deg) = max(abs(steer_angle_deg)) \n")

## Find position where steer_angle_deg == max(steer_angle_deg)
dat4pos.deg_max <- dat2proc %>%
  
  ## ... for each subject_id in each round
  group_by(subject_id, round_txt) %>%
  
  ## ... but only in distance below predefined value (see above)
  ## (only important for situation 3, because of long steering sequence)
  ## ... and only the maximum values
  filter(pxx_dti_m_rnd1 <= set4expl$dist_max) %>%
  
  ## There must be a second filter
  ## ... otherwise there will be no prefiltering and values will be missing
  filter(abs(steer_angle_deg) == max(abs(steer_angle_deg))) %>%
  
  ## ... and then output only the maximum steering angle and one position value
  ## (in case of duplicates with same value take the minimum position)
  ## ... and also the corresponding speed
  summarise(pxx_dti_m_rnd1_min = min(pxx_dti_m_rnd1),
            steer_angle_deg_max = max(steer_angle_deg),
            speed_kmh_max = max(speed_kmh))



cat("* Compute mean and sd for found position values: \n")

## Workaround for dataframe
dat4pos.deg_max <- as.data.frame(dat4pos.deg_max)


## Build average and standard deviations for each round
dat4pos.deg_max.avg_round <-
  dat4pos.deg_max %>%
  
  ## ... for each round
  group_by(round_txt) %>%
  
  ## Compute key values
  summarise(pxx_dti_m_rnd1_min_avg = mean(pxx_dti_m_rnd1_min),
            pxx_dti_m_rnd1_min_sd = sd(pxx_dti_m_rnd1_min),
            steer_angle_deg_max_avg = mean(steer_angle_deg_max),
            steer_angle_deg_max_sd = sd(steer_angle_deg_max),
            speed_kmh_max_avg = mean(speed_kmh_max),
            speed_kmh_max_sd = sd(speed_kmh_max))

## Workaround for dataframe
dat4pos.deg_max.avg_round <- as.data.frame(dat4pos.deg_max.avg_round)

pos4deg_max <- mean(dat4pos.deg_max.avg_round$pxx_dti_m_rnd1_min_avg)



# Explore data ------------------------------------------------------------

if (set4expl$plot == T) {
  
  cat("\n")
  print(dat4pos.deg_max.avg_round)
  cat("\n")
  
  cat("Mean position for maximum steering angle:", pos4deg_max, "\n")
  cat("\n")
  
  
  cat("* Visualisation of (1st) maximum steering angle ... \n")
  
  plotdat.proc <-
    plotdat.proc +
    geom_vline(xintercept = pos4deg_max, size = 1)
  
  plotdat <-
    plotdat.proc +
    ggtitle("Position for average maximum steering angle")
  
  plot(plotdat)
  
  pauseAndContinue()
  
}



# Adjustment process ------------------------------------------------------

## Find position for minumum steering angle before amplitude ----

cat("* Find position where abs(steer_angle_deg) = min(abs(steer_angle_deg)) \n")
cat("* Meaning: Start of steering amplitude \n")

## Find last position ...
dat4pos.deg_min_before <- dat2proc %>%
  
  ## ... where steerangle == min(steer_angle_deg) (see below)
  ## ... but only below the mean position of steerangle peaks
  ## ... but also above a set threshold
  filter(pxx_dti_m_rnd1 <= pos4deg_max) %>%
  filter(pxx_dti_m_rnd1 >=
           min(set4expl$dist_min, pos4deg_max + set4expl$dist_min)) %>%
  
  ## ... for each subject_id and for each round
  group_by(subject_id, round_txt) %>%
  
  # (see explanation above)
  filter(abs(steer_angle_deg) == min(abs(steer_angle_deg))) %>%
  
  ## ... compute values for each subject_id and round (see below)
  ## (same as for dat4pos.deg_max)
  summarise(pxx_dti_m_rnd1_max = max(pxx_dti_m_rnd1),
            steer_angle_deg_min = min(steer_angle_deg),
            speed_kmh_max = max(speed_kmh))


## Workaround for dataframe
dat4pos.deg_min_before <- as.data.frame(dat4pos.deg_min_before)


cat("* Compute mean and sd for found position values: \n")

## Build average and standard deviations for each round
dat4pos.deg_min_before.avg_round <-
  dat4pos.deg_min_before %>%
  group_by(round_txt) %>%
  summarise(pxx_dti_m_rnd1_max_avg = mean(pxx_dti_m_rnd1_max),
            pxx_dti_m_rnd1_max_sd = sd(pxx_dti_m_rnd1_max),
            steer_angle_deg_min_avg = mean(steer_angle_deg_min),
            steer_angle_deg_min_sd = sd(steer_angle_deg_min),
            speed_kmh_avg = mean(speed_kmh_max),
            speed_kmh_sd = sd(speed_kmh_max))


## Workaround for dataframe
dat4pos.deg_min_before.avg_round <-
  as.data.frame(dat4pos.deg_min_before.avg_round)


pos4deg_start <- mean(dat4pos.deg_min_before.avg_round$pxx_dti_m_rnd1_max_avg)



# Explore data ------------------------------------------------------------

if (set4expl$plot == T) {
  
  cat("\n")
  print(dat4pos.deg_min_before.avg_round)
  cat("\n")
  
  cat("Mean position for start of steering amplitude:", pos4deg_start, "\n")
  cat("\n")
  
  
  cat("* Visualisation for start of steering amplitude (orange) ... \n")
  
  plotdat.proc <-
    plotdat.proc +
    geom_vline(xintercept = pos4deg_start, size = 1, colour = "darkorange")
  
  plotdat <-
    plotdat.proc +
    ggtitle("Start of steering amplitude")
  
  plot(plotdat)
  
  pauseAndContinue()
  
}



# Adjustment process ------------------------------------------------------

## Find position for minumum steering angle before amplitude ----

cat("* Again: \n")
cat("* Find position where abs(steer_angle_deg) = min(abs(steer_angle_deg)) \n")
cat("* Meaning: End of steering amplitude \n")

## Find next position where steerangle == min(steer_angle_deg)
dat4pos.deg_min_after <- dat2proc %>%
  
  ## ... but only below the mean position of steerangle peaks
  ## ... but also above a set threshold
  filter(pxx_dti_m_rnd1 >= pos4deg_max) %>%
  
  ## ... compute values for each subject_id and round
  ## (same as for dat4pos.deg_max)
  group_by(subject_id, round_txt) %>%
  
  # (see explanation above)
  filter(abs(steer_angle_deg) == min(abs(steer_angle_deg))) %>%
  
  ## Compute key values
  summarise(pxx_dti_m_rnd1_min = min(pxx_dti_m_rnd1),
            steer_angle_deg_min = min(steer_angle_deg),
            speed_kmh_max = max(speed_kmh))


## Workaround for dataframe
dat4pos.deg_min_after <- as.data.frame(dat4pos.deg_min_after)



cat("* Compute mean and sd for found position values: \n")

## Build average and standard deviations for each round
dat4pos.deg_min_after.avg_round <-
  dat4pos.deg_min_after %>%
  
  ## ... for each round
  group_by(round_txt) %>%
  
  ## Compute key values
  summarise(pxx_dti_m_rnd1_min_avg = mean(pxx_dti_m_rnd1_min),
            pxx_dti_m_rnd1_min_sd = sd(pxx_dti_m_rnd1_min),
            steer_angle_deg_min_avg = mean(steer_angle_deg_min),
            steer_angle_deg_min_sd = sd(steer_angle_deg_min),
            speed_kmh_max_avg = mean(speed_kmh_max),
            speed_kmh_max_sd = sd(speed_kmh_max))


## Workaround for dataframe
dat4pos.deg_min_after.avg_round <-
  as.data.frame(dat4pos.deg_min_after.avg_round)


pos4deg_end <- mean(dat4pos.deg_min_after.avg_round$pxx_dti_m_rnd1_min_avg)



# Explore data ------------------------------------------------------------

if (set4expl$plot == T) {
  
  cat("\n")
  print(dat4pos.deg_min_after.avg_round)
  cat("\n")
  
  cat("Mean position of end for steering amplitude:", pos4deg_end, "\n")
  cat("\n")
  
  
  cat("* Visualisation of end for steering amplitude (orange) ... \n")
  
  plotdat.proc <-
    plotdat.proc +
    geom_vline(xintercept = pos4deg_end, size = 1, colour = "darkorange2")
  
  plotdat <-
    plotdat.proc +
    ggtitle("End of steering amplitude")
  
  plot(plotdat)
  
  pauseAndContinue()
  
}



# Re-Computation ----------------------------------------------------------

## Based on new knowledge about data (approximated position for start)
## ... recompute mean position for maximum
## Exactly the same as first computation in section "Finding process"


## Why is that?
## There might be cases where there are peaks ...
## ... very before and after the actual turning maneouvre


cat("* Recomputation: \n")
cat("* Find position where abs(steer_angle_deg) = max(abs(steer_angle_deg)) \n")
cat("* (old data will be overwritten) \n")


## New threshold, where maximum steering angle is looked for before
threshold_new <-
  pos4deg_max + abs(pos4deg_start - pos4deg_max)/
  set4expl$treshold_factor
#threshold_new <-  pos4deg_max

## Find position where steer_angle_deg == max(steer_angle_deg)
dat4pos.deg_max <- dat2proc %>%
  
  ## ... for each subject_id in each round
  group_by(subject_id, round_txt) %>%
  
  ## ... but only in distance below predefined value
  ## (only important for situation 3, because of long steering sequence)
  ## ... and only the maximum values
  filter(pxx_dti_m_rnd1 <= threshold_new) %>%
  
  ## Former version
  #filter(pxx_dti_m_rnd1 <= set4expl$dist_max &
  ## ... BUT THIS TIME ALSO ...
  ## a) above lower limit (see settings)
  ## b) but also below current mean position + 10 m
  #pxx_dti_m_rnd1 >= pos4deg_start &
  
  ## There must be a second filter
  ## ... otherwise there will be no prefiltering and values will be missing
  filter(abs(steer_angle_deg) == max(abs(steer_angle_deg))) %>%
  
  ## ... and output only the maximum steering angle and one position value
  ## (in case of duplicates with same value take the minimum position)
  ## ... and also the corresponding speed
  summarise(pxx_dti_m_rnd1_min = min(pxx_dti_m_rnd1),
            steer_angle_deg_max = max(steer_angle_deg),
            speed_kmh_max = max(speed_kmh))


## Workaround for dataframe
dat4pos.deg_max <- as.data.frame(dat4pos.deg_max)


cat("* Compute mean and sd for found position values: \n")
cat("* (old data will be overwritten) \n")

## Build average and standard deviations for each round
dat4pos.deg_max.avg_round <-
  dat4pos.deg_max %>%
  group_by(round_txt) %>%
  summarise(pxx_dti_m_rnd1_min_avg = mean(pxx_dti_m_rnd1_min),
            pxx_dti_m_rnd1_min_sd = sd(pxx_dti_m_rnd1_min),
            steer_angle_deg_max_avg = mean(steer_angle_deg_max),
            steer_angle_deg_max_sd = sd(steer_angle_deg_max),
            speed_kmh_max_avg = mean(speed_kmh_max),
            speed_kmh_max_sd = sd(speed_kmh_max))

## Workaround for dataframe
dat4pos.deg_max.avg_round <- as.data.frame(dat4pos.deg_max.avg_round)


pos4deg_max_v2 <- mean(dat4pos.deg_max.avg_round$pxx_dti_m_rnd1_min_avg)



# Explore data ------------------------------------------------------------

if (set4expl$plot == T) {
  
  cat("\n")
  print(dat4pos.deg_max.avg_round)
  cat("\n")
  
  
  cat("* Visualisation of (2nd) maximum steering angle (red) ... \n")
  
  plotdat.proc <-
    plotdat.proc +
    geom_vline(xintercept = pos4deg_max_v2,
               size = 1,
               colour = "red",
               linetype = "dashed")
  
  plotdat <-
    plotdat.proc +
    ggtitle("Position of maximum steering angle (re-computed)")
  
  plot(plotdat)
  
  pauseAndContinue()
  
}

cat("Mean position of maximum steering angle:", pos4deg_max, "\n")
cat("Mean position of maximum steering angle (re-computed):", pos4deg_max_v2, "\n")
cat("\n")


#  ------------------------------------------------------------------------

## Restore full data
dat2proc <- dat2proc_backup

cat("* Processing complete! \n")
cat("* Output key values for starting, maximum and ending position: \n")
cat("\n")




# Output of computed key values -------------------------------------------

cat("Average position of minimum steer_angle_deg before maximum: \n")
cat("---------------------------------------------------------- \n")
print(dat4pos.deg_min_before.avg_round)
cat("\n")

cat("Average position of maximum steer_angle_deg: \n")
cat("------------------------------------------- \n")
print(dat4pos.deg_max.avg_round)
cat("\n")

cat("Average position of minimum steer_angle_deg after maximum: \n")
cat("--------------------------------------------------------- \n")
print(dat4pos.deg_min_after.avg_round)
cat("\n")



# Visualisation of mean positions -----------------------------------------

set4expl$plot <- T

cat("* Visualisation of computed values \n")

## Plot only if wanted ---
if (set4expl$plot == T) {
  
  plotdat.adjusted <-
    plotdat.original +
    
    ## For second run finding the mean position
    geom_vline(xintercept = threshold_new) +
    
    ## Thresholds
    ## Only values above this threshold get computed
    geom_vline(xintercept = set4expl$dist_min, size = 1, linetype = "dotted") +
    ## Only values below this threshold get computed
    geom_vline(xintercept = set4expl$dist_max, size = 1, linetype = "dotted") +
    
    ## Average position with maximum steering angle
    geom_vline(data = dat4pos.deg_max.avg_round,
               aes(xintercept = pxx_dti_m_rnd1_min_avg,
                   group = round_txt),
               col = c("blue",
                       "green",
                       "red")) +
    
    ## Average position for start of amplitude
    geom_vline(data = dat4pos.deg_min_before.avg_round,
               aes(xintercept = pxx_dti_m_rnd1_max_avg,
                   group = round_txt),
               col = c("blue",
                       "green",
                       "red"),
               linetype = rep("dashed", 3)) +
    
    ## Average position for end of amplitude
    geom_vline(data = dat4pos.deg_min_after.avg_round,
               aes(xintercept = pxx_dti_m_rnd1_min_avg,
                   group = round_txt),
               col = c("blue",
                       "green",
                       "red"),
               linetype = rep("dashed", 3)) +
    
    theme_bw() +
    
    ggtitle(paste(paste("Intersection:", set4expl$sit, " - Computed values"),
                  "(black = predefined threshold for starting (left)",
                  "and computed threshold for ending (right)",
                  sep = "\n"))
  
  plot(plotdat.adjusted)
  
}


