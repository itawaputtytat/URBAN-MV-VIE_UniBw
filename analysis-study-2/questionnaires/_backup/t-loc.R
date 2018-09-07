
# Preparatory settings ----------------------------------------------------

sett_dat <- c()
sett_dat$db_conn_name <- dbFindConnObj("URBAN-MV-VIE_UniBw_Study-2")
sett_dat$db_src_name <- "t_q_tloc"
sett_dat$q_name_abbr <- "tloc"
sett_dat$col_name_ind <- "subject_id"



# Obtain additional information for questionnaire -------------------------

initQuestSettings(sett_dat$q_name_abbr, 
                  dbFindConnObj("Questionnaires"), 
                  "T-LOC_Özkan_2005",
                  lang = "ger")



# Load data ---------------------------------------------------------------

dat_q <- dbGetSrc(sett_dat$db_conn_name, 
                  sett_dat$db_src_name,
                  sett_dat$col_name_ind) 



# Find and impute missing values ------------------------------------------

reportNA(dat_q, sett_dat$col_name_ind)
#dat_q <- imputeNA(dat_q, sett_dat$col_name_ind)



# Compute scores ----------------------------------------------------------

dat_q_scores <- 
  computeScores(dat_q, 
                sett_q[[sett_dat$q_name_abbr]]$subscale_items,
                col_name_prefix_overall = sett_q[[sett_dat$q_name_abbr]]$abbr,
                fun = "mean",
                compute_z = T,
                compute_overall = T)



# Gather data -------------------------------------------------------------

dat_q_scores_long <- 
  dat_q_scores %>% 
  select(!!sett_dat$col_name_ind, 
         !!sett_q[[sett_dat$q_name_abbr]]$subscale_names) %>% 
  gather(key = !!sett_dat$col_name_ind) %>% 
  setNames(., c(sett_dat$col_name_ind, "subscale_name", "score"))



# Factorize and label subscale values -------------------------------------

dat_q_scores_long <- 
  dat_q_scores_long %>% 
  mutate(subscale =
           factor(subscale_name,
                  levels = names(sett_q[[sett_dat$q_name_abbr]]$subscale_items),
                  labels = 
                    breakStringToLines(
                      unique(sett_q[[sett_dat$q_name_abbr]]$db_data_lang$subscale_name),
                      max_nwords = 2)) )



# Boxplot -----------------------------------------------------------------

## Adjust values for plotting points and lines
dat_q_scores_long$subscale_jittered <- 1
#jitter(as.numeric(dat_q_scores_long$subscale), factor = 0.4)
dat_q_scores_long$subscale_jittered <- 
  jitter(dat_q_scores_long$subscale_jittered, factor = 3)
dat_q_scores_long$score_jittered <- 
  jitter(dat_q_scores_long$score, factor = 1.5)

plot_box <- 
  ggplot() + 
  geom_boxplot(data = dat_q_scores_long,
               aes(x = subscale,
                   y = score,
                   fill = subscale),
               alpha = 0.25,
               notch = T,
               notchwidth = 0.8) +
  geom_point(data = dat_q_scores_long,
             aes(x = subscale_jittered,
                 y = score_jittered,
                 colour = subscale)) + 
  #facet_grid(subscale~., scales = "free_y", switch = "y") + 
  facet_grid(subscale~., scales = "free_y") + 
  coord_flip(ylim = c(sett_q[[sett_dat$q_name_abbr]]$scale_values_min,
                      sett_q[[sett_dat$q_name_abbr]]$scale_values_max))

plot(plot_box)



# Postprocess boxplot -----------------------------------------------------

plot_box <-
  plot_box + 
  ggtitle("T_LOC: Subscale scores") + 
  labs(x = "Subscale",
       y = "Score",
       fill = "Subscale", 
       colour = "Subscale") +
  guides(fill = F, colour = F) +
  theme_bw() +
  theme(title = element_text(size = 16, face = "bold")) +
  theme(axis.title.x = element_text(size = 15),
        axis.text.x = element_text(size = 15, color = "black")) + 
  theme(axis.title.y = element_text(size = 15),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) + 
  #theme(strip.text.y = element_text(size = 15, angle = 180))
  theme(strip.text.y = element_text(size = 15, angle = 0))

plot(plot_box)

ggsave("plot_box.png",
       plot_box,
       width = 7.5,
       height = 6,
       dpi = 600)



# Compute summary for each item -------------------------------------------

dat_q_items_summary <-
  dat_q %>%
  select(-one_of(!!sett_dat$col_name_ind)) %>%
  gather(key = "item_code", value = "response") %>%
  group_by(item_code) %>%
  summarise(response_mean = mean(response, na.rm = T),
            response_sd = sd(response, na.rm = T))



# Prepare item profile text labels ----------------------------------------

dat_q_items_summary_w_text <- 
  left_join(dat_q_items_summary %>% 
              mutate(id = row_number()),
            sett_q[[sett_dat$q_name_abbr]]$db_data_lang %>% 
              select(id, item_text))

dat_q_items_summary_w_text <- 
  dat_q_items_summary_w_text %>% 
  ## Keep original order of items
  mutate(item_text = factor(item_text, 
                            levels = unique(item_text),
                            labels = 
                              breakStringToLines(item_text, 
                                                 max_nchar = 40)))



# Plot item profile -------------------------------------------------------

plot_item_profile <- 
  plotItemProfile(dat_q_items_summary_w_text,
                  col_name_items = "item_text",
                  value_range = c(sett_q[[sett_dat$q_name_abbr]]$scale_values_min,
                                  sett_q[[sett_dat$q_name_abbr]]$scale_values_max))

plot(plot_item_profile)




# Postprocess item profile ------------------------------------------------

plot_item_profile_post <-
  plot_item_profile +
  ggtitle(label = "T_LOC: Item profile") +
  labs(x = NULL,
       y = "Response mean") +
  theme_bw() +
  theme(legend.justification = c(1, 1),
        legend.position = c(0.999, 0.999),
        legend.background = element_rect(color = "black",
                                         size = 0.2)) +
  scale_x_discrete(position = "top") +
  scale_color_manual(name = "Data Source",
                     values = c("green3", "red2", "grey50")) +
  scale_linetype_manual(name = "Data Source",
                        values = c("solid", "longdash", "dashed")) +
  scale_shape_manual(name = "Data Source",
                     values = c(NA, NA, NA)) +
  scale_size_manual(name = "Data Source",
                    values = rep(0.35, 3)) +
  scale_alpha_manual(name = "Data Source",
                     values = rep(0.35, 3)) +
  theme(title = element_text(size = 16, face = "bold")) +
  theme(axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16)) +
  theme(axis.text.x = element_text(size = 15, color = "black"),
        axis.text.y = element_text(size = 15, color = "black")) +
  theme(legend.key.size = unit(0.2, "cm"),
        legend.margin = margin(rep(0.1, 4), unit='cm'),
        legend.title = element_text(size = 14, color = "black"),
        legend.text = element_text(size = 14, color = "black"))


ggsave("plot_item_profile.png",
       plot_item_profile_post,
       width = 7.5,
       height = 6,
       dpi = 600)

convert_cm_to_px <- function(cm, dpi) {
  px <- cm * dpi / 2.54
}
