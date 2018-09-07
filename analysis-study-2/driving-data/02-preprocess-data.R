
# Settings ----------------------------------------------------------------

sett_preproc <- c()

## Data
sett_preproc$df_name <- sett_query$df_name
sett_preproc$col_names$am <- sett_query$col_name_am
sett_preproc$col_names$speed <- "speed_kmh"
sett_preproc$col_names$speed_ms <- "speed_ms"
sett_preproc$col_names$brake_status <- "brake_status"
sett_preproc$thresholds$am1 <- sett_query$am_limit1
sett_preproc$thresholds$am2 <- sett_query$am_limit2

## Naming
sett_preproc$df_name_intrpld <- paste_(sett_preproc$df_name, "intrpld")
sett_preproc$df_name_intrpld_cut <- paste_(sett_preproc$df_name_intrpld, "cut")



# Preprocess --------------------------------------------------------------

## Interpolation
intrpldf_batch4rb(sett_preproc$df_name, 
                  col_name_ref = sett_query$col_name_am, 
                  binary_vars = sett_preproc$col_names$brake_status,
                  suffix = "intrpld", 
                  outputFlag = T)

## Filter data to distance
cut2dist_batch4rb(sett_preproc$df_name_intrpld,
                  sett_preproc$col_names$am,
                  sett_preproc$thresholds$am1,
                  sett_preproc$thresholds$am2)

## Calculate speed (ms)
assign(sett_preproc$df_name_intrpld_cut, 
       get(sett_preproc$df_name_intrpld_cut) %>% 
         mutate_(.dots = setNames(list(
           interp(~ v / 3.6,
                  v = as.name(sett_preproc$col_names$speed))),
           sett_preproc$col_names$speed_ms)))