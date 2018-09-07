
# Preparatory settings ----------------------------------------------------

sett_dat <- c()
sett_dat$db_conn_name <- dbFindConnObj("URBAN-MV-VIE_UniBw_Study-2")
sett_dat$db_src_name <- "v_q_driving_style"
sett_dat$q_name_abbr <- "ds"
sett_dat$col_name_ind <- "subject_id"



# Obtain additional information for questionnaire -------------------------

initQuestSettings(sett_dat$q_name_abbr, 
                  dbFindConnObj("Questionnaires"), 
                  "Driving-style_Arndt_2011_adapted",
                  lang = "ger")



# Load data ---------------------------------------------------------------

dat_q <- dbGetSrc(sett_dat$db_conn_name, 
                  sett_dat$db_src_name,
                  sett_dat$col_name_ind) 



# Find and impute missing values ------------------------------------------

reportNA(dat_q, sett_dat$col_name_ind)
#dat_q <- imputeNA(dat_q, sett_dat$col_name_ind)



# Gather data -------------------------------------------------------------

dat_q_long <- 
  dat_q %>% 
  select(!!sett_dat$col_name_ind, 
         !!sett_q[[sett_dat$q_name_abbr]]$item_col_names) %>% 
  gather(key = !!sett_dat$col_name_ind) %>% 
  setNames(., c(sett_dat$col_name_ind, "item_col_name", "value"))



# Factorize and label subscale values -------------------------------------

dat_q_long <- 
  dat_q_long %>% 
  mutate(item =
           factor(item_col_name,
                  levels = sett_q[[sett_dat$q_name_abbr]]$item_col_names,
                  labels = sett_q[[sett_dat$q_name_abbr]]$db_data_lang$item_text)) %>% 
  separate(item, c("item_left", "item_right"), "; ", remove = F) %>% 
  arrange(subject_id, item_col_name) %>% 
  group_by(subject_id) %>% 
  mutate(item_nr = row_number())



# Boxplot -----------------------------------------------------------------

geom_violin <- 
  ggplot() + 
  geom_violin(data = dat_q_long,
               aes(x = item_nr,
                   y = value,
                   fill = item,
                   colour = item),
               alpha = 0.25) +
  geom_jitter(data = dat_q_long,
              aes(x = item_nr,
                  y = value,
                  color = item),
              position = position_jitter(height = 0.1, width = 0.05)) + 
  scale_x_continuous(breaks = 1:max(dat_q_long$item_nr),
                     labels = unique(dat_q_long$item_left),
                     sec.axis = sec_axis(~.,
                                         breaks = 1:max(dat_q_long$item_nr),
                                         labels = unique(dat_q_long$item_right))) + 
  coord_flip(ylim = c(sett_q[[sett_dat$q_name_abbr]]$scale_values_min,
                      sett_q[[sett_dat$q_name_abbr]]$scale_values_max))

plot(geom_violin)



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
