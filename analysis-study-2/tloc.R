
# Load data ---------------------------------------------------------------

dat_tloc <- dbGetSrc(dbconn_study2, "t_q_tloc")



# Compute scores ----------------------------------------------------------

scales2compute <- 
  c("tloc_os", ## Own skill
    "tloc_ob", ## Own behaviour
    "tloc_od", ## Other driver
    "tloc_ve", ## Vehicle or environment
    "tloc_f") ## Fate

items4scales <- 
  list(paste0("tloc", sprintf("%02d", c(1, 2))),
       paste0("tloc", sprintf("%02d", c(7, 9, 16))),
       paste0("tloc", sprintf("%02d", c(3, 4, 8, 10, 14, 15))),
       paste0("tloc", sprintf("%02d", c(6, 12, 13))),
       paste0("tloc", sprintf("%02d", c(5, 11, 17))))

dat_tloc <- 
  computeScores(dat_tloc, 
                scales2compute, 
                items4scales, 
                "mean",
                compZ = T)

dat_tloc$tloc_overall <- rowMeans(dat_tloc[, unlist(items4scales)])
dat_tloc$tloc_overall.z <- scale(dat_tloc$tloc_overall, center = T, scale = T)



# Gather data -------------------------------------------------------------

dat_tloc.long <- 
  dat_tloc %>% 
  select(subid, tloc_os:tloc_f) %>% 
  gather(key = subid) %>% 
  setNames(., c("subid", "scale", "score")) %>% 
  mutate(scale = factor(scale, 
                        levels = scales2compute,
                        labels = c("Own skill",
                                   "Own behaviour",
                                   "Other driver",
                                   "Vehicle or environment",
                                   "Fate")))



# Boxplot -----------------------------------------------------------------

## Adjust values for plotting points and lines
dat_tloc.long$scale.jittered <- 
  jitter(as.numeric(dat_tloc.long$scale), factor = 0.4)
dat_tloc.long$score.jittered <- 
  jitter(dat_tloc.long$score, factor = 1.5)

ggplot() + 
  geom_boxplot(data = dat_tloc.long,
               aes(x = scale,
                   y = score,
                   fill = scale),
               alpha = 0.25,
               notch = T, notchwidth = 0.8) +
  geom_point(data = dat_tloc.long,
             aes(x = scale.jittered,
                 y = dat_tloc.long$score.jittered,
                 colour = scale)) + 
  geom_line(data = dat_tloc.long,
            aes(x = scale.jittered,
                y = dat_tloc.long$score.jittered,
                group = subid),
            alpha = 0.25) + 
  theme_bw() + 
  ggtitle("BSSS") + 
  labs(x = "Scale",
       y = "Score",
       fill = "Scale", colour = "Scale")



# Scale correlation -------------------------------------------------------

library(psych)
pairs.panels(dat_tloc[, scales2compute], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)
