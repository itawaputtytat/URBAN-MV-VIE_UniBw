
# Settings ----------------------------------------------------------------

sett <- list()
sett$db$db_names$study2 <- "URBAN-MV-VIE_UniBw_Study-2"
sett$db$db_names$quest <- "Questionnaires"
sett$db$conn_names$study2 <- dbFindConnObj(sett$db$db_names$study2, output = F)
sett$db$conn_names$quest <- dbFindConnObj(sett$db$db_names$quest, output = F)
sett$db$src_names$vip <- "t_q_vip_screening"
sett$db$src_names$vip_norm <- "VIP_Schmidt_2002_Norm"

initQuestSettings("vip", 
                  sett$db$conn_names$quest, 
                  "VIP_Schmidt_2012",
                  lang = "ger")



# Query data --------------------------------------------------------------

dat_vip <- 
  dbGetSrc(sett$db$conn_names$study2,
           sett$db$src_names$vip)

dat_vip_norm <- 
  dbGetSrc(sett$db$conn_names$quest,
           sett$db$src_names$vip_norm)



# Check for missing values ------------------------------------------------

md.pattern(dat_vip)



# Preprocess data ---------------------------------------------------------

dat_vip <-
  recodeItems(dat_vip,
              sett$quest$vip$items$col_names_to_reverse,
              sett$quest$vip$scale$values_min,
              sett$quest$vip$scale$values_max)

dat_vip_long <- 
  reshapeLong(dat_vip, col_names_to_keep = "screening_subject_id")

dat_vip_long_summary <- 
  computeSummary(dat_vip_long)

dat_vip_scores <- 
  computeScores(dat_vip,
                sett$quest$vip$subscales$items,
                fun = "sum")

dat_vip_norm <-
  dat_vip_norm %>% 
  arrange(percentile_rank)

## Add information on percentile rank and interpretation
for (i in grep("vip", names(dat_vip_scores), value = T)) {
  row_finder <- 
    sapply(dat_vip_scores[, i], 
           function(x) { 
             max(which(dat_vip_norm[, i] <= x)) 
             } )
  row_finder[is.infinite(row_finder)] <- 1
  dat_vip_scores[[paste_(i, "pr")]] <- 
    dat_vip_norm[row_finder, "percentile_rank"]
  dat_vip_scores[[paste_(i, "pr_txt")]] <- 
    dat_vip_norm[row_finder, "interpretation_abbr"]
}

## Reshape data to long format for multiple columns requires additional steps
## Columns must be separated and spreaded again
dat_vip_scores_long <- 
  dat_vip_scores %>% 
  reshapeLong("variable", "value", "screening_subject_id") %>% 
  mutate(subscale = substr(variable, 1, 6)) %>% 
  mutate(variable_type = 
           ifelse(grepl("pr", variable), "pr", "value")) %>% 
  mutate(variable_type = 
           ifelse(grepl("txt", variable), "interpretation", variable_type)) %>% 
  mutate(variable = NULL) %>% 
  spread(variable_type, value) %>% 
  select(screening_subject_id, subscale, value, pr, interpretation) %>% 
  mutate(value = as.numeric(value),
         pr = as.numeric(pr))



# Save data ---------------------------------------------------------------

dbWriteTable(get(sett$db$conn_names$study2),
             "t_q_vip_screening_scores",
             dat_vip_scores,
             row.names = F,
             overwrite = T)



# Prepare data for subscale plot ------------------------------------------

dat_vip_scores_long <- 
  convertSubscaleToFactor(dat_vip_scores_long,
                          sett$quest$vip$subscales$col_names_unique,
                          sett$quest$vip$subscales$names_lang,
                          sett$quest$vip$meta$lang)


# Visualize scores --------------------------------------------------------

plot_subscales <- 
  ggplot() + 
  geom_violin(data = dat_vip_scores_long,
              aes(x = subscale_fct,
                  y = pr, 
                  fill = subscale_fct,
                  color = subscale_fct),
              alpha = 0.1,
              size = 0.2) +
  geom_boxplot(data = dat_vip_scores_long,
               aes(x = subscale_fct,
                   y = pr,
                   fill = subscale_fct),
               alpha = 0.5,
               width = 0.1,
               fatten = 3,
               size = 0.2,
               outlier.size = 0.5,
               outlier.alpha = 1) +
  coord_flip(ylim = c(0,100))

plot(plot_subscales)

dat_vip_scores_long$interpretation <- 
  factor(dat_vip_scores_long$interpretation,
         levels <- c("bw", "bw_a", "a", "a_aa", "aa"))

ggplot() + 
  geom_bar(data = dat_vip_scores_long,
           aes(interpretation)) + 
  facet_grid(.~subscale) + 
  ggtitle("VIP values interpretation")



# Post process subscales plot ---------------------------------------------

plot_subscales_post <- 
  plot_subscales + 
  
  ## Scale
  scale_x_discrete(position = "top") +
  scale_fill_manual(values = rep(convRGB2Hex(47, 84, 150, 1), 4)) + 
  scale_color_manual(values = rep(convRGB2Hex(47, 84, 150, 1), 4)) +
  #scale_fill_manual(values = rev(RColorBrewer::brewer.pal(5, "Set1")[1:4])) +
  #scale_color_manual(values = rev(RColorBrewer::brewer.pal(5, "Set1")[1:4])) +
  
  ## Label
  ggtitle(label = "Verkehrsspezifischer Itempool",
          subtitle = "Subskalen") + 
  labs(x = NULL, #"Subskalen",
       y = "Prozentrang") +
  guides(color = F, linetype = F, shape = F, size = F, alpha = F, fill = F) +
  
  ## Theme
  theme_thesis()

plot(plot_subscales_post)

ggsave(filename = figureFileName("vip_subskalen"),
       plot = plot_subscales_post,
       path = figurePath(),
       width = 8,
       height = 7,
       units = "cm",
       dpi = 1000,
       type = "cairo-png")



# Prepare data for item profile -------------------------------------------

## Add item texts
dat_vip_long_summary_items <- 
  left_join(dat_vip_long_summary,
            sett$quest$vip$items$texts) %>% 
  mutate(subscales = sett$quest$vip$subscales$col_names) %>% 
  arrange(desc(subscales)) %>% 
  mutate(item_text_fct = 
           factor(item_text, 
                  levels = item_text,
                  labels = breakStringToLines(item_text, max_nchar = 42) )) 


# Visualize item profile --------------------------------------------------

## Plot item profile
plot_profile_ai <- 
  plotItemProfile(dat_vip_long_summary_items %>% 
                    filter(subscales == "vip_ai"),
                  col_name_items = "item_text_fct",
                  xlim = c(1.09, 9.91),
                  ylim = c(0, 1.2))

## Add errorbars on deepest level
plot_profile_ai$layers <- 
  c(geom_errorbar(data = dat_vip_long_summary_items %>% 
                    filter(subscales == "vip_ai"),
                  aes_string(x = "item_text_fct",
                             ymin = "mean - ci",
                             ymax = "mean + ci"),
                  color = "grey50",
                  size = 0.3,
                  width = 0.2),
    plot_profile_ai$layers)

## Add subscale information
plot_profile_ai <- 
  plot_profile_ai + 
  geom_rect(data = dat_vip_long_summary_items %>% 
              filter(subscales == "vip_ai"),
            aes(xmin = as.numeric(item_text_fct) - 0.5 - (49-10), 
                xmax = as.numeric(item_text_fct) + 0.5 - (49-10), 
                ymin = sett$quest$vip$scale$values_max + 0.2, 
                ymax = sett$quest$vip$scale$values_max + 2,
                fill = subscales),
            alpha = 0.5) + 
  #scale_fill_brewer(palette = "Set1")
  scale_fill_manual(values = rev(RColorBrewer::brewer.pal(5, "Set1")[1]))

plot(plot_profile_ai)



## Plot item profile
plot_profile_ed <- 
  plotItemProfile(dat_vip_long_summary_items %>% 
                    filter(subscales == "vip_ed"),
                  col_name_items = "item_text_fct",
                  xlim = c(1.09, 10.91),
                  ylim = c(0, 1.2))

## Add errorbars on deepest level
plot_profile_ed$layers <- 
  c(geom_errorbar(data = dat_vip_long_summary_items %>% 
                    filter(subscales == "vip_ed"),
                  aes_string(x = "item_text_fct",
                             ymin = "mean - ci",
                             ymax = "mean + ci"),
                  color = "grey50",
                  size = 0.3,
                  width = 0.2),
    plot_profile_ed$layers)

## Add subscale information
plot_profile_ed <- 
  plot_profile_ed + 
  geom_rect(data = dat_vip_long_summary_items %>% 
              filter(subscales == "vip_ed"),
            aes(xmin = as.numeric(item_text_fct) - 0.5 - (49-10-11), 
                xmax = as.numeric(item_text_fct) + 0.5 - (49-10-11), 
                ymin = sett$quest$vip$scale$values_max + 0.2, 
                ymax = sett$quest$vip$scale$values_max + 2,
                fill = subscales),
            alpha = 0.5) + 
  #scale_fill_brewer(palette = "Set1")
  scale_fill_manual(values = rev(RColorBrewer::brewer.pal(5, "Set1")[2]))

plot(plot_profile_ed)



## Plot item profile
plot_profile_sd <- 
  plotItemProfile(dat_vip_long_summary_items %>% 
                    filter(subscales == "vip_sd"),
                  col_name_items = "item_text_fct",
                  xlim = c(1.09, 7.91),
                  ylim = c(0, 1.2))

## Add errorbars on deepest level
plot_profile_sd$layers <- 
  c(geom_errorbar(data = dat_vip_long_summary_items %>% 
                    filter(subscales == "vip_sd"),
                  aes_string(x = "item_text_fct",
                             ymin = "mean - ci",
                             ymax = "mean + ci"),
                  color = "grey50",
                  size = 0.3,
                  width = 0.2),
    plot_profile_sd$layers)

## Add subscale information
plot_profile_sd <- 
  plot_profile_sd + 
  geom_rect(data = dat_vip_long_summary_items %>% 
              filter(subscales == "vip_sd"),
            aes(xmin = as.numeric(item_text_fct) - 0.5 - (49-10-11-8), 
                xmax = as.numeric(item_text_fct) + 0.5 - (49-10-11-8), 
                ymin = sett$quest$vip$scale$values_max + 0.2, 
                ymax = sett$quest$vip$scale$values_max + 2,
                fill = subscales),
            alpha = 0.5) + 
  #scale_fill_brewer(palette = "Set1")
  scale_fill_manual(values = rev(RColorBrewer::brewer.pal(5, "Set1")[3]))

plot(plot_profile_sd)




## Plot item profile
plot_profile_us <- 
  plotItemProfile(dat_vip_long_summary_items %>% 
                    filter(subscales == "vip_us"),
                  col_name_items = "item_text_fct",
                  xlim = c(1.09, 19.91),
                  ylim = c(0, 1.2))

## Add errorbars on deepest level
plot_profile_us$layers <- 
  c(geom_errorbar(data = dat_vip_long_summary_items %>% 
                    filter(subscales == "vip_us"),
                  aes_string(x = "item_text_fct",
                             ymin = "mean - ci",
                             ymax = "mean + ci"),
                  color = "grey50",
                  size = 0.3,
                  width = 0.2),
    plot_profile_us$layers)

## Add subscale information
plot_profile_us <- 
  plot_profile_us + 
  geom_rect(data = dat_vip_long_summary_items %>% 
              filter(subscales == "vip_us"),
            aes(xmin = as.numeric(item_text_fct) - 0.5 - (49-10-11-8-20), 
                xmax = as.numeric(item_text_fct) + 0.5 - (49-10-11-8-20), 
                ymin = sett$quest$vip$scale$values_max + 0.2, 
                ymax = sett$quest$vip$scale$values_max + 2,
                fill = subscales),
            alpha = 0.5) + 
  #scale_fill_brewer(palette = "Set1")
  scale_fill_manual(values = rev(RColorBrewer::brewer.pal(5, "Set1")[4]))

plot(plot_profile_us)




# Post-process item profile -----------------------------------------------

plot_profile_ai_post <- 
  plot_profile_ai +
  
  ## Scale
  scale_color_manual(values = convRGB2Hex(47, 84, 150, 1)) + 
  scale_linetype_manual(values = "solid") +
  scale_shape_manual(values = 20) +
  scale_size_manual(values = 0.35) + 
  scale_alpha_manual(values = 0.35) + 
  
  ## Labels
  ggtitle(label = "Verkehrsspezifischer Itempool",
          subtitle = "Subskala: Aggressive Interaktion") + 
  labs(x = NULL,
       y = "Mittelwerte (95% KI)") +
  scale_x_discrete(position = "top") +
  guides(color = F, linetype = F, shape = F, size = F, alpha = F, fill = F) +
  
  ## Theme
  theme_thesis()

plot(plot_profile_ai_post)

ggsave(filename = figureFileName("vip_item-profile_ai"), 
       plot = plot_profile_ai_post,
       path = figurePath(),
       width = 8,
       height = 10,
       units = "cm",
       dpi = 1000,
       type = "cairo-png")



plot_profile_ed_post <- 
  plot_profile_ed +
  
  ## Scale
  scale_color_manual(values = convRGB2Hex(47, 84, 150, 1)) + 
  scale_linetype_manual(values = "solid") +
  scale_shape_manual(values = 20) +
  scale_size_manual(values = 0.35) + 
  scale_alpha_manual(values = 0.35) + 
  
  ## Labels
  ggtitle(label = "Verkehrsspezifischer Itempool",
          subtitle = "Subskala: Emotionales Autofahren") + 
  labs(x = NULL,
       y = "Mittelwerte (95% KI)") +
  scale_x_discrete(position = "top") +
  guides(color = F, linetype = F, shape = F, size = F, alpha = F, fill = F) +
  
  ## Theme
  theme_thesis()

plot(plot_profile_ed_post)

ggsave(filename = figureFileName("vip_item-profile_ed"), 
       plot = plot_profile_ed_post,
       path = figurePath(),
       width = 8,
       height = 10,
       units = "cm",
       dpi = 1000,
       type = "cairo-png")



plot_profile_sd_post <- 
  plot_profile_sd +
  
  ## Scale
  scale_color_manual(values = convRGB2Hex(47, 84, 150, 1)) + 
  scale_linetype_manual(values = "solid") +
  scale_shape_manual(values = 20) +
  scale_size_manual(values = 0.35) + 
  scale_alpha_manual(values = 0.35) + 
  
  ## Labels
  ggtitle(label = "Verkehrsspezifischer Itempool",
          subtitle = "Subskala: Soziale Erwünschtheit") + 
  labs(x = NULL,
       y = "Mittelwerte (95% KI)") +
  scale_x_discrete(position = "top") +
  guides(color = F, linetype = F, shape = F, size = F, alpha = F, fill = F) +
  
  ## Theme
  theme_thesis()

plot(plot_profile_sd_post)

ggsave(filename = figureFileName("vip_item-profile_sd"), 
       plot = plot_profile_sd_post,
       path = figurePath(),
       width = 8,
       height = 8,
       units = "cm",
       dpi = 1000,
       type = "cairo-png")



plot_profile_us_post <- 
  plot_profile_us +
  
  ## Scale
  scale_color_manual(values = convRGB2Hex(47, 84, 150, 1)) + 
  scale_linetype_manual(values = "solid") +
  scale_shape_manual(values = 20) +
  scale_size_manual(values = 0.35) + 
  scale_alpha_manual(values = 0.35) + 
  
  ## Labels
  ggtitle(label = "Verkehrsspezifischer Itempool",
          subtitle = "Subskala: Unkritische Selbstwahrnehmung") + 
  labs(x = NULL,
       y = "Mittelwerte (95% KI)") +
  scale_x_discrete(position = "top") +
  guides(color = F, linetype = F, shape = F, size = F, alpha = F, fill = F) +
  
  ## Theme
  theme_thesis()

plot(plot_profile_us_post)

ggsave(filename = figureFileName("vip_item-profile_us"), 
       plot = plot_profile_us_post,
       path = figurePath(),
       width = 8,
       height = 16,
       units = "cm",
       dpi = 1000,
       type = "cairo-png")
