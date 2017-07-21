
# Load data ---------------------------------------------------------------

dat_bsss <- dbGetSrc(dbconn_study2, "t_q_bsss")



# Compute scores ----------------------------------------------------------

scales2compute <- 
  c("bsss_e", ## Experience seeking
    "bsss_b", ## Boredom susceptibility
    "bsss_t", ## Thrill and adventure seeking
    "bsss_d") ## Disinhibition

items4scales <- 
  list(paste0("bsss", sprintf("%02d", c(1, 5))),
       paste0("bsss", sprintf("%02d", c(2, 6))),
       paste0("bsss", sprintf("%02d", c(3, 7))),
       paste0("bsss", sprintf("%02d", c(4, 8))))

dat_bsss <- 
  computeScores(dat_bsss, 
                scales2compute, 
                items4scales, 
                "mean",
                compZ = T)

dat_bsss$bsss_overall <- rowMeans(dat_bsss[, unlist(items4scales)])
dat_bsss$bsss_overall.z <- scale(dat_bsss$bsss_overall, center = T, scale = T)



# Gather data -------------------------------------------------------------

dat_bsss.long <- 
  dat_bsss %>% 
  select(subid, bsss_e:bsss_d) %>% 
  gather(key = subid) %>% 
  setNames(., c("subid", "scale", "score")) %>% 
  mutate(scale = factor(scale, 
                        levels = scales2compute,
                        labels = c("Experience seeking",
                                   "Boredom susceptibility",
                                   "Thrill and adventure seeking",
                                   "Disinhibition")))



# Boxplot -----------------------------------------------------------------

## Adjust values for plotting points and lines
dat_bsss.long$scale.jittered <- 
  jitter(as.numeric(dat_bsss.long$scale), factor = 0.4)
dat_bsss.long$score.jittered <- 
  jitter(dat_bsss.long$score, factor = 1.5)

ggplot() + 
  geom_boxplot(data = dat_bsss.long,
               aes(x = scale,
                   y = score,
                   fill = scale),
               alpha = 0.25,
               notch = T, notchwidth = 0.8) +
  geom_point(data = dat_bsss.long,
             aes(x = scale.jittered,
                 y = dat_bsss.long$score.jittered,
                 colour = scale)) + 
  geom_line(data = dat_bsss.long,
            aes(x = scale.jittered,
                y = dat_bsss.long$score.jittered,
                group = subid),
            alpha = 0.25) + 
  theme_bw() + 
  ggtitle("BSSS") + 
  labs(x = "Scale",
       y = "Score",
       fill = "Scale", colour = "Scale")



# Scale correlation -------------------------------------------------------

library(psych)
pairs.panels(dat_bsss[, scales2compute], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)
