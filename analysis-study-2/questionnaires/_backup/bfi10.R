
# Load data ---------------------------------------------------------------

dat_bfi <- dbGetSrc(dbconn_study2, "t_q_bfi10")



# Recode items ------------------------------------------------------------

items2recode <- names(dat_bfi)[c(1, 3, 4, 5, 7) + 1]
dat_bfi <- recodeItems(dat_bfi, items2recode, 5)



# Compute scores ----------------------------------------------------------

## Compute scores
scales2compute <- 
  c("bfi_e", 
    "bfi_n", 
    "bfi_o", 
    "bfi_c", 
    "bfi_a")

items4scales <- 
  list(paste0("bfi", sprintf("%02d", c(1, 6))),
       paste0("bfi", sprintf("%02d", c(4, 9))),
       paste0("bfi", sprintf("%02d", c(5, 10))),
       paste0("bfi", sprintf("%02d", c(3, 8))),
       paste0("bfi", sprintf("%02d", c(2, 7))))

dat_bfi <- 
  computeScores(dat_bfi, 
                scales2compute, 
                items4scales, 
                "mean",
                compZ = T)



# Gather data -------------------------------------------------------------

dat_bfi.long <- 
  dat_bfi %>% 
  select(subid, bfi_e:bfi_a) %>% 
  gather(key = subid) %>% 
  setNames(., c("subid", "scale", "score")) %>% 
  mutate(scale = factor(scale, 
                        levels = scales2compute,
                        labels = c("Extraversion",
                                   "Neurotiscm",
                                   "Openness",
                                   "Consiousness",
                                   "Agreeableness")))



# Boxplot -----------------------------------------------------------------

## Adjust values for plotting points and lines
dat_bfi.long$scale.jittered <- 
  jitter(as.numeric(dat_bfi.long$scale), factor = 0.4)
dat_bfi.long$score.jittered <- 
  jitter(dat_bfi.long$score, factor = 1.5)

ggplot() + 
  geom_boxplot(data = dat_bfi.long,
               aes(x = scale,
                   y = score,
                   fill = scale),
               alpha = 0.25,
               notch = T, notchwidth = 0.8) +
  geom_point(data = dat_bfi.long,
             aes(x = scale.jittered,
                 y = dat_bfi.long$score.jittered,
                 colour = scale)) + 
  geom_line(data = dat_bfi.long,
            aes(x = scale.jittered,
                y = dat_bfi.long$score.jittered,
                group = subid),
            alpha = 0.25) + 
  theme_bw() + 
  ggtitle("BFI-10") + 
  labs(x = "Scale",
       y = "Score",
       fill = "Scale", colour = "Scale")



# Scale correlation -------------------------------------------------------

library(psych)
pairs.panels(dat_bfi[, scales2compute], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)
