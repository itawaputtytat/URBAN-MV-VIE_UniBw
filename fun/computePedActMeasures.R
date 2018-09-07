computePedActMeasures <- function(
  dat,
  col_name_id,
  col_names_group,
  col_names_measure,
  col_name_am,
  col_name_am_start,
  col_name_am_end,
  col_name_pedal_act = "pedal_act",
  col_name_pedal_int_perc = "pedal_int_perc",
  col_name_pedal_id = "pedal_act_id",
  treshold,
  threhshold_before_braking
) {
  
  ## Summary of pedal activity
  dat_summary <- 
    computePedActSummary(
      dat,
      col_name_id = col_name_id,
      col_name_am = col_name_am,
      col_name_am_start = col_name_am_start,
      col_name_am_end = col_name_am_end
    )
  
  ## Add summary to data
  dat <- 
    left_join(dat,
              dat_summary %>% 
                select_(col_name_id, 
                        col_name_pedal_id, 
                        paste_(col_name_pedal_act, "nr")))
  
  ## Reduce data
  dat_reduced <- 
    dat %>% 
    select_(.dots = c(
      col_names_group,
      col_name_am,
      col_names_measure,
      col_name_pedal_act,
      col_name_pedal_int_perc
    )) 
  
  
  
  ## FIRST ACCELERATION
  
  ## Find first acceleration activity after threshold
  dat_reduced_acc_t2 <- 
    findPedActSeqAccT2(dat_summary,
                       col_name_id = col_name_id,
                       col_name_am_start = col_name_am_start,
                       col_name_am_end = col_name_am_end,
                       threshold = treshold)
  
  ## Order activities 
  dat_reduced_acc_t2_order <- 
    reorderPedalSeqIds(dat_reduced_acc_t2, 
                       col_name_id = col_name_id, 
                       col_name_to_order = col_name_am_start)
  
  ## Merge data and activities
  dat_acc_t2 <-
    left_join(dat_reduced,
              dat_reduced_acc_t2 %>% 
                select_(col_name_id,
                        paste_(col_name_pedal_act, "nr_acc_t2") ))
  
  ## Re-order data
  dat_acc_t2[, col_name_id] <- 
    factor(dat_acc_t2[, col_name_id], 
           levels = dat_reduced_acc_t2_order)
  
  
  
  ## RELEASE ACCELERATION
  
  dat_reduced_acc_t1 <-
    left_join(dat_summary,
              dat_reduced_acc_t2 %>%
                select_(col_name_id,
                        paste_(col_name_pedal_act, "nr_acc_t2") ))

  dat_reduced_acc_t1 <-
    findPedActSeqAccT1(
      dat_reduced_acc_t1,
      col_names_id = col_name_id,
      col_names_am_start = col_name_am_start,
      col_names_am_end = col_name_am_end,
      threshold = threhshold_before_braking)

  dat_reduced_acc_t1_order <-
    reorderPedalSeqIds(dat_reduced_acc_t1,
                       col_name_id = col_name_id,
                       col_name_to_order = col_name_am_end)

  ## Merge data and last previous acclerating activity before braking
  dat_acc_t1 <-
    left_join(dat_acc_t2,
              dat_reduced_acc_t1 %>%
                select_(col_name_id,
                        paste_(col_name_pedal_act, "nr_acc_last_before_brake")),
              by = col_name_id)

  dat_acc_t1[, col_name_id] <-
    factor(dat_acc_t1[, col_name_id],
           levels = dat_reduced_acc_t1_order)

  
  
  ## FIRST BRAKING
  
  dat_reduced_brake_1st <- 
    left_join(dat_summary,
              dat_reduced_acc_t1 %>% 
                select_(col_name_id, 
                        paste_(col_name_pedal_act, "nr_acc_last_before_brake"))) 
  
  dat_reduced_brake_1st <- 
    findPedActSeqFirstBraking(dat_reduced_brake_1st,
                              col_name_id = col_name_id,
                              col_name_am_start = col_name_am_start)
  
  dat_reduced_brake_1st_order <- 
    reorderPedalSeqIds(dat_reduced_brake_1st, 
                       col_name_id = col_name_id, 
                       col_name_to_order = col_name_am_start)
  
  
  ## Merge data and last previous acclerating activity before braking
  dat_brake_1st <-
    left_join(dat_acc_t1,
              dat_reduced_brake_1st %>% 
                select_(col_name_id, 
                        paste_(col_name_pedal_act, "nr_brake_first")),
              by = col_name_id)
  
  dat_brake_1st[, col_name_id] <- 
    factor(dat_brake_1st[, col_name_id], 
           levels = dat_reduced_brake_1st_order)
  
  return(list(
    dat_summary = dat_summary,
    dat = dat,
    dat_reduced = dat_reduced,
    dat_reduced_acc_t2 = dat_reduced_acc_t2,
    dat_reduced_acc_t2_order = dat_reduced_acc_t2_order,
    dat_acc_t2 = dat_acc_t2,
    dat_reduced_acc_t1 = dat_reduced_acc_t1,
    dat_reduced_acc_t1_order = dat_reduced_acc_t1_order,
    dat_acc_t1 = dat_acc_t1,
    dat_reduced_brake_1st = dat_reduced_brake_1st,
    dat_reduced_brake_1st_order = dat_reduced_brake_1st_order,
    dat_brake_1st = dat_brake_1st
  ))
}