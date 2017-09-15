dat4test <- 
  t_adtf_dist_m_rnd1_full.intrpl.cut %>% 
  filter(passing == "s04_intro_subject03")
plot(dat4test$speed_kmh / 3.6, ylim = c(0, 30), type = "l")

#acclat_ms2.max <- max(dat4test$acclat_ms2)
acclat_ms2.max <- 3.5 
lines(v, col = "grey50")
v1 <- sqrt( 2 / dat4curv_curv$curv ) 
v2 <- sqrt( 2.75 / dat4curv_curv$curv ) 
v3 <- sqrt( 3 / dat4curv_curv$curv ) 
v1[which(v > 48/3.6)] <- 48/3.6
v2[which(v > 54/3.6)] <- 54/3.6
v3[which(v > 60/3.6)] <- 60/3.6
lines(v1, col = "grey")
lines(v2, col = "grey")
lines(v3, col = "grey")

dat4test$v1 <- v1
dat4test$v2 <- v2
dat4test$v3 <- v3

dat4test$model1_gradient_ms <- dat4test$v1
dat4test$model2_gradient_ms <- dat4test$v2
dat4test$model3_gradient_ms <- dat4test$v3

dat4test$pxx_dist_m_rnd1 <- round(dat4test$pxx_dist_m_rnd1, 1)

dat4test <-
  dat4test %>%
  mutate(time_s_diff = time_s - lag(time_s),
         dist_m_diff = pxx_dist_m_rnd1 - lag(pxx_dist_m_rnd1)) %>%
  mutate(time_s_diff.synth1 = dist_m_diff / v1,
         time_s_diff.synth2 = dist_m_diff / v2,
         time_s_diff.synth3 = dist_m_diff / v3) %>%
  mutate(time_s_diff.synth1_v2 = round(dist_m_diff / 13.333, 5),
         time_s_diff.synth2_v2 = round(dist_m_diff / 15, 5),
         time_s_diff.synth3_v2 = round(dist_m_diff / 16.667, 5))

k = which(dat4curv_curv$curv == max(dat4curv_curv$curv)) - 1
#for(k in c(k:(k-50))) {
for(k in c(k:1)) {
  #cat(k, "\n")
  #dat4test$model1_gradient[k-1] <- ((dat4test$model1_gradient[k]) + (dat4test$model1_gradient[which(temp == max(temp))]) * 0.15/30.6)
  #dat4test$model1_gradient_ms[k-1] <-  dat4test$model1_gradient_ms[k] + dat4test$model1_gradient_ms[k] * 0.15 * dat4test$time_s_diff[k]
  # dat4test$model1_gradient_ms[k-1] <-  dat4test$model1_gradient_ms[k] + dat4test$model1_gradient_ms[k] * 0.15 * dat4test$dist_m_rnd1_diff[k]/10
  # dat4test$model2_gradient_ms[k-1] <-  dat4test$model2_gradient_ms[k] + dat4test$model2_gradient_ms[k] * 0.20 * dat4test$dist_m_rnd1_diff[k]/10
  # dat4test$model3_gradient_ms[k-1] <-  dat4test$model3_gradient_ms[k] + dat4test$model3_gradient_ms[k] * 0.25 * dat4test$dist_m_rnd1_diff[k]/10
  
  dat4test$model1_gradient_ms[k-1] <-  dat4test$model1_gradient_ms[k] + dat4test$model1_gradient_ms[k] * 0.15 * dat4test$time_s_diff[k]
  dat4test$model2_gradient_ms[k-1] <-  dat4test$model2_gradient_ms[k] + dat4test$model2_gradient_ms[k] * 0.20 * dat4test$time_s_diff[k]
  dat4test$model3_gradient_ms[k-1] <-  dat4test$model3_gradient_ms[k] + dat4test$model3_gradient_ms[k] * 0.25 * dat4test$time_s_diff[k]
  
  # dat4test$model1_gradient_ms[k-1] <-  dat4test$model1_gradient_ms[k] + dat4test$model1_gradient_ms[k] * 0.15 *  0.008915
  # dat4test$model2_gradient_ms[k-1] <-  dat4test$model2_gradient_ms[k] + dat4test$model2_gradient_ms[k] * 0.20 *  0.008915
  # dat4test$model3_gradient_ms[k-1] <-  dat4test$model3_gradient_ms[k] + dat4test$model3_gradient_ms[k] * 0.25 *  0.008915
  
  # dat4test$model1_gradient_ms[k-1] <-  dat4test$model1_gradient_ms[k] + dat4test$model1_gradient_ms[k] * 0.15 * dat4test$time_s_diff.synth1[k]
  # dat4test$model2_gradient_ms[k-1] <-  dat4test$model2_gradient_ms[k] + dat4test$model2_gradient_ms[k] * 0.20 * dat4test$time_s_diff.synth2[k]
  # dat4test$model3_gradient_ms[k-1] <-  dat4test$model3_gradient_ms[k] + dat4test$model3_gradient_ms[k] * 0.25 * dat4test$time_s_diff.synth3[k]
  
  # dat4test$model1_gradient_ms[k-1] <-  dat4test$model1_gradient_ms[k] + dat4test$model1_gradient_ms[k] * 0.15 * dat4test$time_s_diff.synth1_v2[k]
  # dat4test$model2_gradient_ms[k-1] <-  dat4test$model2_gradient_ms[k] + dat4test$model2_gradient_ms[k] * 0.20 * dat4test$time_s_diff.synth2_v2[k]
  # dat4test$model3_gradient_ms[k-1] <-  dat4test$model3_gradient_ms[k] + dat4test$model3_gradient_ms[k] * 0.25 * dat4test$time_s_diff.synth3_v2[k]
  
  # dat4test$model1_gradient_ms[k-1] <-  dat4test$model1_gradient_ms[k] + dat4test$model1_gradient_ms[k] * 0.15 * 0.01
  # dat4test$model2_gradient_ms[k-1] <-  dat4test$model2_gradient_ms[k] + dat4test$model2_gradient_ms[k] * 0.20 * 0.01
  # dat4test$model3_gradient_ms[k-1] <-  dat4test$model3_gradient_ms[k] + dat4test$model3_gradient_ms[k] * 0.25 * 0.01
  
  # dat4test$model1_gradient_ms[k-1] <-  dat4test$model1_gradient_ms[k] + dat4test$model1_gradient_ms[k] * 0.15
  # dat4test$model2_gradient_ms[k-1] <-  dat4test$model2_gradient_ms[k] + dat4test$model2_gradient_ms[k] * 0.20
  # dat4test$model3_gradient_ms[k-1] <-  dat4test$model3_gradient_ms[k] + dat4test$model3_gradient_ms[k] * 0.25
}


dat4test$model1_gradient_ms[which(dat4test$model1_gradient_ms > 13.333)] <- 13.333
dat4test$model2_gradient_ms[which(dat4test$model2_gradient_ms > 15)] <- 15
dat4test$model3_gradient_ms[which(dat4test$model3_gradient_ms > 16.667)] <- 16.667

# dat4test$model1_gradient_ms[which(dat4test$model1_gradient_ms > 13.333)] <- min(dat4test$model1_ms, 13.333)
# dat4test$model2_gradient_ms[which(dat4test$model2_gradient_ms > 15)] <- min(dat4test$model2_ms, 15)
# dat4test$model3_gradient_ms[which(dat4test$model3_gradient_ms > 16.667)] <- min(dat4test$model3_ms, 16.667)

lines(dat4test$model1_gradient_ms, col = "blue")
lines(dat4test$model2_gradient_ms, col = "green")
lines(dat4test$model3_gradient_ms, col = "red")




clustoutput <-
  #clust2groups(dat4clust.clean.spread.filtered[, min:max],
  kmeanspp(dat4clust.clean.spread,
               k = sett_proc$k, 
               start = centretest,
               iter.max = 1,
               nstart = 1)

