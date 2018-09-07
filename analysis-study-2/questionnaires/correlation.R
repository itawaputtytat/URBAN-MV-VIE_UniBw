
# Settings ----------------------------------------------------------------

sett <- list()
sett$db$db_names$study2 <- "URBAN-MV-VIE_UniBw_Study-2"
sett$db$conn_names$study2 <- dbFindConnObj(sett$db$db_names$study2, output = F)
sett$db$src_names$demo <- "v_q_demography"
sett$db$src_names$exp <- "v_q_driving_experience"
sett$db$src_names$tloc <- "t_q_tloc_scores"  
sett$db$src_names$bsss <- "t_q_bsss_scores" 
sett$db$src_names$ds <- "t_q_ds_scores" 
sett$db$src_names$bfi10 <- "t_q_bfi10_scores" 
sett$db$src_names$vip <- "v_q_vip_scores"



# Query data --------------------------------------------------------------

dat_demo <- dbGetSrc(sett$db$conn_names$study2, sett$db$src_names$demo)
dat_exp <- dbGetSrc(sett$db$conn_names$study2, sett$db$src_names$exp)
dat_tloc <- dbGetSrc(sett$db$conn_names$study2, sett$db$src_names$tloc)
dat_bsss <- dbGetSrc(sett$db$conn_names$study2, sett$db$src_names$bsss)
dat_ds <- dbGetSrc(sett$db$conn_names$study2, sett$db$src_names$ds)
dat_bfi10 <- dbGetSrc(sett$db$conn_names$study2, sett$db$src_names$bfi10)
dat_vip <- dbGetSrc(sett$db$conn_names$study2, sett$db$src_names$vip)



# Preprocess data ---------------------------------------------------------

dat_exp <- 
  dat_exp %>% 
  mutate(driving_licence_years = 2015 - driving_licence_year) %>% 
  mutate(driving_licence_years = 
           ifelse(subject_id == 19,
                  2015 - (2015-driving_licence_year),
                  driving_licence_years)) %>% 
  mutate(driving_frequency_per_week = 
           factor(driving_frequency_per_week,
                  labels = c(1, 3, 5, 7))) %>% 
  mutate(driving_frequency_per_week = 
           as.numeric(
             as.character(driving_frequency_per_week)
             )) %>% 
  select(subject_id,
         driving_licence_years,
         regular_driving_years,
         km_overall,
         km_recent_year,
         driving_frequency_per_week)
  
dat_tloc <-
  dat_tloc %>% 
  select(subject_id,
         tloc_f,
         tloc_od,
         tloc_s,
         tloc_ve)

dat_vip <- 
  dat_vip %>% 
  select(subject_id,
         vip_ai_pr,
         vip_ed_pr,
         vip_sd_pr,
         vip_us_pr)

dat_all <- left_join(dat_exp, dat_tloc)
dat_all <- left_join(dat_all, dat_bsss)
dat_all <- left_join(dat_all, dat_ds)
dat_all <- left_join(dat_all, dat_bfi10)
dat_all <- left_join(dat_all, dat_vip)
# dat_all <- 
#   apply(dat_all, 2, function(x) {
#     if (is.character(x)) {
#       x = as.numeric(as.factor(x))
#     }
#   }) %>% 
#   data.frame()


# Correlation visualization -----------------------------------------------

library(psych)
## Visually (only works for selection of variables)
pairs.panels(
  dat_all[, 2:ncol(dat_all)],
  method = "pearson",
  hist.col = "#00AFBB",
  density = TRUE,
  ellipses = TRUE,
  stars = TRUE
)



# Correlation matrix ------------------------------------------------------

## Compute correlation matrix
results <- corr.test2(dat_all[,2:ncol(dat_all)])

results_df <- results$ci
results_df$pair_name <- rownames(results_df)
results_df <- 
  results_df %>%
  tidyr::separate(pair_name, c("var_name1", "var_name2"), sep = "-", remove = FALSE) %>% 
  select(var_name1, var_name2, r, p, lower, upper, pair_name)
results_df$r <- round(results_df$r, 2)
results_df$p<- round(results_df$p, 3)
results_df$lower <- round(results_df$lower, 2)
results_df$upper <- round(results_df$upper, 2)

dat_r_bfi10_bsss <-
  results_df %>% 
  filter(grepl("bfi10", pair_name)) %>% 
  filter(grepl("bsss", pair_name)) %>% 
  arrange(var_name2) %>% 
  select(BFI10 = var_name2, BSSS = var_name1, r, p, lower, upper)

write.table(test, "clipboard", sep="\t", row.names=FALSE, quote = FALSE)

dat_r_bfi10_vip <-
  results_df %>% 
  filter(grepl("bfi10", pair_name)) %>% 
  filter(grepl("vip", pair_name)) %>% 
  arrange(var_name1) %>% 
  select("BFI-10" = var_name1, VIP = var_name2, r, p, lower, upper)

write.table(dat_r_bfi10_vip, "clipboard", sep="\t", row.names=FALSE, quote = FALSE)


dat_r_bfi10_tloc <-
  results_df %>% 
  filter(grepl("bfi10", pair_name)) %>% 
  filter(grepl("tloc", pair_name)) %>% 
  arrange(var_name2) %>% 
  select("BFI-10" = var_name2, "T-LOC" = var_name1, r, p, lower, upper)

write.table(dat_r_bfi10_tloc, "clipboard", sep="\t", row.names=FALSE, quote = FALSE)

dat_r_bfi10_ds <-
  results_df %>% 
  filter(grepl("bfi10", pair_name)) %>% 
  filter(grepl("ds", pair_name)) %>% 
  arrange(var_name2) %>% 
  select(BFI10 = var_name2, DS = var_name1, r, p, lower, upper) %>% 
  mutate(DS = substr(DS, 4, 5)) %>% 
  mutate(color = ifelse(r > 0, "green3", "red")) %>% 
  mutate(color_p = ifelse(p < 0.05, "blue", "black")) %>% 
  #mutate(text = paste0("r = ", r, "\n p = ", p)) %>% 
  mutate(text_r = paste0("r = ", r)) %>% 
  mutate(text_p = paste0("p = ", p)) %>% 
  mutate(pos_text = r + sign(r) * 0.08)

write.table(dat_r_bfi10_ds, "clipboard", sep="\t", row.names=FALSE, quote = FALSE)



# Viz ---------------------------------------------------------------------

var <- "bfi10_c"

dat_r_plot <- 
  dat_r_bfi10_ds %>% 
  filter(BFI10 == var) %>% 
  arrange(r)

order <- dat_r_plot %>% distinct(DS)
order <- unlist(order, use.names = FALSE)
order <- rev(order)

dat_r_plot <- 
  left_join(
    dat_r_plot,
    sett$quest$ds$subscales$overview %>% 
      select(DS = subscale_name_eng_abbr,
             subscale_name_ger) %>% 
      mutate(DS = 
               tolower(DS))
  )

dat_r_plot$DS <- factor(dat_r_plot$DS, levels = order, labels = dat_r_plot$subscale_name_ger)

size = 1.75

plot_subscales_post <- 
  ggplot() + 
  geom_bar(data = dat_r_plot,
           aes(x = DS,
               y = r,
               fill = color),
           stat = "identity",
           fill = dat_r_plot$color) + 
  geom_text(data = dat_r_plot %>% 
              filter(color == "green3"),
            aes(x = DS,
                y = pos_text,
                label = text_r,
                color = color_p),
            vjust = -0.3,
            hjust = 0,
            size = size,
            fontface = "bold",
            color = unlist(dat_r_plot %>% filter(color == "green3") %>% select(color_p))) + 
  geom_text(data = dat_r_plot %>%
              filter(color == "green3"),
            aes(x = DS,
                y = pos_text,
                label = text_p,
                color = color_p),
            vjust = 1.3,
            hjust = 0,
            size = size,
            color = unlist(dat_r_plot %>% filter(color == "green3") %>% select(color_p)))  +
  geom_text(data = dat_r_plot %>% 
              filter(color == "red"),
            aes(x = DS,
                y = pos_text - 0.35,
                label = text_r,
                color = color_p),
            vjust = -0.3,
            hjust = 0,
            size = size,
            fontface = "bold",
            color = unlist(dat_r_plot %>% filter(color == "red") %>% select(color_p))) + 
  geom_text(data = dat_r_plot %>%
              filter(color == "red"),
            aes(x = DS,
                y = pos_text - 0.35,
                label = text_p,
                color = color_p),
            vjust = 1.3,
            hjust = 0,
            size = size,
            color = unlist(dat_r_plot %>% filter(color == "red") %>% select(color_p))) +
  coord_flip(ylim = c(-1, 1)) +
  theme_thesis() + 
  ggtitle("Korrelation von BFI-10 und Fahrstil",
         subtitle = paste0("BFI-10 Skala: ", var)) + 
  labs(x = NULL, #"Subskalen",
       y = "Korrelation") +
  guides(color = F) + 
  scale_x_discrete(position = "top")


ggsave(filename = figureFileName(paste_("correlation", var, "ds")),
       plot = plot_subscales_post,
       path = figurePath(),
       width = 8,
       height = 7,
       units = "cm",
       dpi = 1000,
       type = "cairo-png")

