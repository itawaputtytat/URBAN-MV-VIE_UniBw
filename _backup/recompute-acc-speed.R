# Re-compute acceleration values ------------------------------------------

# ## Re-compute acceleration
# dat_synth <-
#   dat_synth %>%
#   mutate(acc_lon_ms2_by_speed_ms =
#            (speed_ms - lag(speed_ms)) /
#            (time_s - lag(time_s)) )
# 
# ## Compare recorded and re-computed acceleration values
# ggplot() + 
#   geom_line(data = dat_synth,
#             aes_string(x = sett_synth$col_name_am,
#                        y = "acc_lon_ms2",
#                        group = sett_synth$col_name_group),
#             col = "green3") +
#   geom_line(data = dat_synth,
#             aes_string(x = sett_synth$col_name_am,
#                        y = "acc_lon_ms2_by_speed_ms",
#                        group = sett_synth$col_name_group),
#             col = "red3")
# 
# # Compare recorded and re-computed speed values
# dat_synth <-
#   dat_synth %>%
#   group_by_(sett_synth$col_name_group) %>%
#   mutate(speed_ms_by_acc_lon_ms = speed_ms) %>%
#   mutate(speed_ms_by_acc_lon_ms =
#            acc_lon_ms2 * (time_s - lag(time_s)) +
#            lag(speed_ms_by_acc_lon_ms))
# 
# ggplot() +
#   geom_line(data = dat_synth,
#             aes_string(x = sett_synth$col_name_am,
#                        y = "speed_ms",
#                        group = sett_synth$col_name_group),
#             col = "green3") +
#   geom_line(data = dat_synth,
#             aes_string(x = sett_synth$col_name_am,
#                        y = "speed_ms_by_acc_lon_ms",
#                        group = sett_synth$col_name_group),
#             col = "red3",
#             alpha = 0.5)
# 
# 
# test <- dat_synth %>% filter(passing =="p02_normal_s07")
# plot(test$acc_lon_ms2, type = "l")
# lines(test$acc_lon_ms2_by_speed_ms, col = "green2")
# 
# plot(test$speed_ms, type = "l")
# lines(test$speed_ms_by_acc_lon_ms, col = "green2")
# 
# plot(test$speed_ms - test$speed_ms_by_acc_lon_ms, type = "l")
