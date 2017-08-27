## Code pedalactitivy
##  1 = acceleration pedal
##  0 = no pedal actitivy
## -1 = braking pedal

codePedalActivity <- function(dat,
                              dfnames_prefix, 
                              dfnames_suffix = "",
                              brakepress_bar_treshold = 60) {
  
  outputFunProc(R)
  ptm <- proc.time()
  
  ## Find matching object names
  #dfnames <-
  #  findObjNames(txt2incl = c(dfnames_prefix, dfnames_suffix))

  #   threshold_acc   <- 0
  #   threshold_brake <- 0.3
  #
  #   for(dfname in dfname) {
  #
  #     data <- get(dfname)
  #     data$pedalcode <- 0
  #     data$pedalcode[data$brakepress_bar > threshold_brake] <- -1
  #     data$pedalcode[data$accpedalpos_perc_corr > threshold_acc] <- 1
  #     data$pedalcode <- as.factor(data$pedalcode)
  #
  #     ## create new dataframe
  #     assign(paste(dfname, sep = "_"),
  #            data,
  #            envir = .GlobalEnv)
  #    }
  
  ## pedal activity for acceleration must be coded after braking activity!
  ## see subid 21 in s04
  ## Loop through dfnames
  #for(dfname in dfnames) {
    
  name4obj <- paste(deparse(substitute(dat)))
    outputString(paste("* Current processing:", name4obj, "... "), linebreak = F)
    
    ## Get data
    #dat <- get(dfname)
    
    ## Initialise pedal activity
    dat$pedal_act <- 0
    
    ## Code pedal activity as -1 (for braking) when brakepress_status == 1
    ## No pedal activity will result in: 0 * -1
    dat$brakepress_status <- 
      vapply(dat$brakepress_status, 
             function(x) ifelse(x, 1, 0), FUN.VALUE = numeric(1))
    dat$pedal_act <- dat$brakepress_status * -1
    
    ## Code pedal activity as 1 for accelerating when:
    ## ... pedal_act has not been coded as -1 (for braking) before
    rowfinder <-
      which(dat$pedal_act != -1 & dat$accpedalpos_perc_corr > 0)
    dat$pedal_act[rowfinder] <- 1
    
    ## Initialise pedal intensity
    dat$pedal_int <- 0

    ## Code pedal intensity for braking as percentage to 60 bar
    dat$pedal_int[dat$pedal_act == -1] <-
      100/brakepress_bar_treshold * dat$brakepress_bar[dat$pedal_act == -1]

    ## Code pedal intensity for accelerating (already in percentage)
    dat$pedal_int[dat$pedal_act == 1] <-
      dat$accpedalpos_perc_corr[dat$pedal_act == 1]

    ## Code pedal intensity for no pedal activity as zero
    ## ... not really necessary
    dat$pedal_int[dat$pedal_act == 0] <- 0
    
    ## Code every pedal intensity below zero as zero
    ## Reason: Pedal position for accelerating pedal might be below zero
    dat$pedal_int[dat$pedal_int < 0] <- 0
    
    ## Save data
    assign(name4obj, dat, envir = .GlobalEnv)
  #}
  
    outputDone(T)
  outputString("** New columns: pedal_act, pedal_int")    
  
  outputProcTime(ptm)
}
