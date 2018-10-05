
# Derive function names for initialising BN -------------------------------

## Function name for initialising BN settings
sett_pred$fun_names$initSettingsForBN <- 
  paste_("predLiebner_initSettingsForBN", 
         sett_pred$bn_version)

## Function name for initialising BN
sett_pred$fun_names$predLiebner_initBN <- 
  paste_("predLiebner_initBN", 
         sett_pred$bn_version)



# Prepare settings for BN evidences ---------------------------------------

## For Liebner version remove any evidence
if (sett_pred$bn_version == "Liebner") {
  sett_pred$bn_evidence$nodes <- c()
  sett_pred$bn_evidence$states <- c()
} 

## Prepare nodes for evidence
## Set evidence for ST and DS
if (sett_pred$bn_version != "Liebner") {
  sett_pred$bn_evidence$nodes <- c("ST", "DS")
}

## Identify driver state
if (!sett_pred$bn_evidence$overwrite & 
    sett_pred$bn_version != "Liebner") {
  
  if (grepl("v50", sett_case$case_id)) {
    sett_pred$bn_evidence$states <- c("no_stress")
  } else {
    sett_pred$bn_evidence$states <- c("stress")
  }
  
}

## Detect driving style
if (!sett_pred$bn_evidence$overwrite & 
    sett_pred$bn_version != "Liebner") {
  
  ## Extract subject id
  sett_case$subject_id <- 
    regmatches(sett_case$case_id, 
               regexpr("s\\d\\d", sett_case$case_id))
  sett_case$subject_id <- substr(sett_case$subject_id, 2, 3)
  sett_case$subject_id <- as.numeric(sett_case$subject_id)
  
  ## Fetch data for driving style from database
  dat_ds <- dbGetSrc(sett_ds$db$conn_name, sett_ds$db$src_names$dat_ds)
  
  ## Extract driving style
  sett_case$driving_style <- 
    dat_ds %>% 
    filter(subject_id == sett_case$subject_id) %>% 
    pull(cluster_group_ordered)
  
  ## Calculate prior for driving style
  dat_priors_ds <- 
    dat_ds %>% 
    mutate(n = n()) %>% 
    group_by_(sett_ds$col_names$class) %>% 
    summarize(p = n()/max(n)) %>% 
    pull(p)
  
  ## Set names for driving style
  if (sett_case$driving_style == 1) { sett_case$driving_style <- "sporty" }
  if (sett_case$driving_style == 2) { sett_case$driving_style <- "moderate" }
  if (sett_case$driving_style == 3) { sett_case$driving_style <- "comfortable" }
  
  ## Add detected driving styles to state settings
  sett_pred$bn_evidence$states <- 
    c(sett_pred$bn_evidence$states,
      sett_case$driving_style)
}

## Add observation node to text output
sett_pred$bn_evidence$print <- 
  paste(sett_pred$bn_evidence$nodes, 
        sett_pred$bn_evidence$states, sep = ": ")

if (length(sett_pred$bn_evidence$print) == 0) {
  sett_pred$bn_evidence$print <- "-"
}



# Load DSM ----------------------------------------------------------------

if (sett_dsm$version == "clustering") {
  
  sett_dsm$db$src_names$dsm <- 
    sub(regmatches(sett_dsm$db$src_names$dsm, 
                   regexpr("p\\d\\d", 
                           sett_dsm$db$src_names$dsm)),
        regmatches(sett_case$case_id, regexpr("p\\d\\d", sett_case$case_id)),
        sett_dsm$db$src_names$dsm)
  
  dat_dsm <- 
    dbGetSrc(sett_dsm$db$conn_name, 
             sett_dsm$db$src_names$dsm)
}

if (sett_dsm$version == "curvature") {

  sett_dsm$db$src_names$curvature <- 
    sub(regmatches(sett_dsm$db$src_names$curvature, 
                   regexpr("p\\d\\d", 
                           sett_dsm$db$src_names$curvature)),
        regmatches(sett_case$case_id, regexpr("p\\d\\d", sett_case$case_id)),
        sett_dsm$db$src_names$curvature)
  
  dat_curv <- 
    dbGetSrc(sett_dsm$db$conn_name, 
             sett_dsm$db$src_names$curvature)
  
  ## Limit AM range
  dat_curv <- 
    dat_curv %>% 
    filter_(paste(sett_case$col_names$am, ">=", -100, "&", 
                  sett_case$col_names$am, "<=", 25))
  
  ## Compute DSM
  dat_dsm <- 
    computeDSMFromPathCurvature(
      dat_curv,
      col_name_am = sett_case$col_names$am,
      col_name_curv = "curv_norm",
      u_max = sett_dsm$thresholds$u_max,
      acc_lat_max = sett_dsm$thresholds$acc_lon_max,
      gradients = sett_dsm$thresholds$gradients)
   
  dat_dsm <- 
    dat_dsm %>% 
    select(one_of(sett_dsm$col_names$am),
           ends_with("gr")) %>% 
    gather("variable", "value", -one_of(sett_dsm$col_names$am)) %>% 
    mutate_(.dots = setNames(list(
      interp(~ substr(v, 2, 2),
             v = as.name("variable"))),
      sett_dsm$col_names$cluster_group)) %>% 
    rename_at("value", funs(paste0(sett_dsm$col_names$speed))) %>% 
    mutate(variable = NULL)
}

dat_dsm_spread <- 
  predLiebner_initDSM(
    dat_dsm,
    col_name_cluster_group = sett_dsm$col_names$cluster_group,
    col_name_am = sett_dsm$col_names$am,
    col_name_speed = sett_dsm$col_names$speed,
    threshold_am = sett_dsm$thresholds$u_max_am,
    thresholds_u_max = sett_dsm$thresholds$u_max)



# Load priors for DSM -----------------------------------------------------

## Change source name according to pxx based on case id
sett_dsm$db$src_names$priors_speed <- 
  sub(regmatches(sett_dsm$db$src_names$priors_speed, 
                 regexpr("p\\d\\d", 
                         sett_dsm$db$src_names$priors_speed)),
      regmatches(sett_case$case_id, regexpr("p\\d\\d", sett_case$case_id)),
      sett_dsm$db$src_names$priors_speed)

## Load priors
if (sett_pred$bn_version == "Liebner") {

  dat_priors_speed <- 
    dbGetSrc(sett_dsm$db$conn_name, 
             sett_dsm$db$src_names$priors_speed) %>% 
    mutate(names = M)

}

if (sett_pred$bn_version == "A_S_DS_ST") {
  
  dat_priors_speed <-
    dbGetSrc(sett_dsm$db$conn_name,
             paste_(sett_dsm$db$src_names$priors_speed, "speed_ds",
                    paste0("k", sett_ds$n_ds))) %>% 
    mutate_(.dots = setNames(list(
      interp(~ ifelse(v == 50, "no_stress", "stress"),
             v = as.name(sett_dsm$col_names$condition_speed))),
      "stress")) %>% 
    mutate(names = paste_(M, stress, driving_style))
  
  dat_priors_speed <-
    dat_priors_speed %>%
    arrange(desc(stress), driving_style, M)
  
}



# if (grepl("_S_", sett_pred$bn_version)) {
# 
#   dat_priors_speed <-
#     dbGetSrc(sett_dsm$db$conn_name,
#              paste_(sett_dsm$db$src_names$priors_speed, "speed")) %>% 
#     mutate_(.dots = setNames(list(
#       interp(~ ifelse(v == 50, "no_stress", "stress"),
#              v = as.name(sett_dsm$col_names$condition_speed))),
#       "stress")) %>% 
#     mutate(names = paste_(M, stress))
# 
#   dat_priors_speed <-
#     dat_priors_speed %>%
#     arrange(desc(stress), M)


    
    ## Fake data
    # dat_priors_speed <- rbind(v_probs, v_probs, v_probs)
    # dat_priors_speed$driving_style <-
    #   c(paste0("group", rep(1, 6)),
    #     paste0("group", rep(2, 6)),
    #     paste0("group", rep(3, 6)))
    # dat_priors_speed$rate <- runif(3*6)
# }

dat_priors_speed_rates <- dat_priors_speed %>% pull(rate)
names(dat_priors_speed_rates) <- dat_priors_speed$names



# Load priors for max. lon. acc. ------------------------------------------

## Change source name according to pxx based on case id
sett_dsm$db$src_names$priors_acc_lon_max <- 
  sub(regmatches(sett_dsm$db$src_names$priors_acc_lon_max, 
                 regexpr("p\\d\\d", 
                         sett_dsm$db$src_names$priors_acc_lon_max)),
      regmatches(sett_case$case_id, regexpr("p\\d\\d", sett_case$case_id)),
      sett_dsm$db$src_names$priors_acc_lon_max)

if (sett_pred$bn_version == "Liebner") {
  
  dat_priors_acc_lon_max <-
    dbGetSrc(sett_dsm$db$conn_name,
             sett_dsm$db$src_names$priors_acc_lon_max) %>%
    mutate(name = paste_(M, a))
}

if (sett_pred$bn_version == "A_S_DS_ST") {
  
  dat_priors_acc_lon_max <-
    dbGetSrc(sett_dsm$db$conn_name,
             paste_(sett_dsm$db$src_names$priors_acc_lon_max, "speed_ds",
                    paste0("k", sett_ds$n_ds))) %>%
    mutate_(.dots = setNames(list(
      interp(~ ifelse(v == 50, "no_stress", "stress"),
             v = as.name(sett_dsm$col_names$condition_speed))),
      "stress")) %>% 
    mutate_(.dots = setNames(list(
      interp(~ ifelse(v == 1, "sporty", 
                      ifelse(v == 2, "moderate", "comfortable")),
             v = as.name(sett_dsm$col_names$driving_style))),
      "driving_style"))
  
  dat_priors_acc_lon_max <-
    dat_priors_acc_lon_max %>% 
    mutate(names = paste_(M, a, stress, driving_style)) %>%
    arrange(M, desc(stress), driving_style, a)
  
}

# if (grepl("A", sett_pred$bn_version)) { 
# 
#   dat_priors_acc_lon_max <-
#     dbGetSrc(sett_dsm$db$conn_name,
#              sett_dsm$db$src_names$priors_acc_lon_max) %>%
#     mutate_(.dots = setNames(list(
#       interp(~ ifelse(v == 50, "no_stress", "stress"),
#              v = as.name(sett_dsm$col_names$condition_speed))),
#       "stress"))
#   
#   dat_priors_acc_lon_max <-
#     dat_priors_acc_lon_max %>% 
#     mutate(names = paste_(M, a, stress)) %>%
#     arrange(M, desc(stress), a)
  

    
    ## Fake data
    # dat_priors_acc_lon_max <- do.call("rbind", replicate(3, acc_max, simplify = FALSE))
    # dat_priors_acc_lon_max$driving_style <-
    #   c(paste0("group", rep(1, 18)),
    #     paste0("group", rep(2, 18)),
    #     paste0("group", rep(3, 18)))
    # dat_priors_acc_lon_max$rate <- runif(3*18)
# }

## Name values
dat_priors_acc_lon_max_rates <- dat_priors_acc_lon_max %>% pull(rate)
names(dat_priors_acc_lon_max_rates) <- dat_priors_acc_lon_max$name



# Initialise IDM settings -------------------------------------------------

source("fun_Liebner_2013/settings/sett_idm.R")



# Initialise BN components ------------------------------------------------

if (sett_pred$bn_version == "Liebner") {
  
  sett_bn <- 
    do.call(sett_pred$fun_names$initSettingsForBN, 
            list(priors_speed = dat_priors_speed_rates,
                 priors_acc_lon_max = dat_priors_acc_lon_max_rates,
                 length_speed = length(unique(dat_priors_speed$M)),
                 length_acc = length(unique(dat_priors_acc_lon_max$a))))
  
} 

if (sett_pred$bn_version == "A_S_DS_ST") {
  
  sett_bn <- 
    do.call(sett_pred$fun_names$initSettingsForBN, 
            list(priors_speed = dat_priors_speed_rates,
                 priors_acc_lon_max = dat_priors_acc_lon_max_rates,
                 priors_DS = dat_priors_ds,
                 length_speed = length(unique(dat_priors_speed$M)),
                 length_acc = length(unique(dat_priors_acc_lon_max$a)),
                 length_DS = length(unique(dat_priors_ds)),
                 state_names_DS = sett_ds$ds_names))
  
}

bn <- 
  do.call(sett_pred$fun_names$predLiebner_initBN, 
          list(sett_bn))



# Initialise collector for P_O_Hi -----------------------------------------

coll_prob_template <- 
  idm_createSimDat(
    list(Ij = sett_bn$states_n$I,
         Mk = sett_bn$states_n$S,
         al = sett_bn$states_n$A),
    id_order = sett_bn$id_order)



# Further settings --------------------------------------------------------

## Plot
#sett_plot$sys_os_sysname <- unlist(Sys.info())["sysname"]
#sett_plot$sys_os_release <- unlist(Sys.info())["release"]
#sett_plot$screen_width <- getScreenResolution()$width
#sett_plot$screen_height <- getScreenResolution()$height
sett_plot$colors$dsm <- RColorBrewer::brewer.pal(9, "Set1")[1:sett_bn$states_n$S]
sett_plot$colors$dsm <- paste0(sett_plot$colors$dsm, 50)
sett_plot$colors$prob <- c("#6FCDDD", "orange", "#ED2125", "#B9539F")
names(sett_plot$colors$prob) <- sett_bn$state_names$I
sett_plot$line_width$prob <- 2
sett_plot$n_simulations <- sett_bn$states_n$S * sett_bn$states_n$A
