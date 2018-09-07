
# Settings ----------------------------------------------------------------

sett <- list()
sett$db$db_names$study2 <- "URBAN-MV-VIE_UniBw_Study-2"
sett$db$db_names$quest <- "Questionnaires"
sett$db$conn_names$study2 <- dbFindConnObj(sett$db$db_names$study2, output = F)
sett$db$conn_names$quest <- dbFindConnObj(sett$db$db_names$quest, output = F)
sett$db$src_names$tloc <- "t_q_tloc"

sett$quest <- 
  initQuestSettings("tloc", 
                    sett$db$conn_names$quest, 
                    "T-LOC_Özkan_2005",
                    lang = "ger")



# Query data --------------------------------------------------------------

dat_tloc <- 
  dbGetSrc(sett$db$conn_names$study2,
           sett$db$src_names$tloc)



# Check for missing values ------------------------------------------------

md.pattern(dat_tloc)



# Preprocess data ---------------------------------------------------------

dat_tloc_long <- 
  reshapeLong(dat_tloc, col_names_to_keep = "subject_id")

dat_tloc_long_summary <- 
  computeSummary(dat_tloc_long)

dat_tloc_scores <- 
  computeScores(dat_tloc,
                sett$quest$tloc$subscales$items)

dat_tloc_scores_long <- 
  reshapeLong(dat_tloc_scores, "subscale", "value", "subject_id")



# Save data ---------------------------------------------------------------

dbWriteTable(get(sett$db$conn_names$study2),
             "t_q_tloc_scores",
             dat_tloc_scores,
             row.names = F,
             overwrite = T)



# Prepare data for subscale plot ------------------------------------------

dat_tloc_scores_long <- 
  convertSubscaleToFactor(dat_tloc_scores_long,
                          sett$quest$tloc$subscales$col_names_unique,
                          sett$quest$tloc$subscales$names_lang,
                          sett$quest$tloc$meta$lang)



# Visualize scores --------------------------------------------------------

plot_subscales <- 
  ggplot() + 
  geom_violin(data = dat_tloc_scores_long,
              aes(x = subscale_fct,
                  y = value, 
                  fill = subscale_fct,
                  color = subscale_fct),
              alpha = 0.1,
              size = 0.2) +
  geom_boxplot(data = dat_tloc_scores_long,
               aes(x = subscale_fct,
                   y = value,
                   fill = subscale_fct),
               alpha = 0.5,
               width = 0.1,
               fatten = 3,
               size = 0.2,
               outlier.size = 0.5,
               outlier.alpha = 1) +
  coord_flip(ylim = c(1,5))

plot(plot_subscales)



# Post process subscales plot ---------------------------------------------

plot_subscales_post <- 
  plot_subscales + 
  
  ## Scake
  scale_x_discrete(position = "top") +
  scale_fill_manual(values = rep(convRGB2Hex(47, 84, 150, 1), 5)) + 
  scale_color_manual(values = rep(convRGB2Hex(47, 84, 150, 1), 5)) +
  # scale_fill_manual(values = rev(RColorBrewer::brewer.pal(5, "Set1")[1:5])) +
  # scale_color_manual(values = rev(RColorBrewer::brewer.pal(5, "Set1")[1:5])) +
  
  ## Label
  ggtitle(label = "Traffic Locus of Control",
          subtitle = "Subskalen") + 
  labs(x = NULL, #"Subskalen",
       y = "Skalen-Mittelwert") +
  guides(color = F, linetype = F, shape = F, size = F, alpha = F, fill = F) +
  
  ## Theme
  theme_thesis() 

plot(plot_subscales_post)

ggsave(filename = figureFileName("tloc_subskalen"),
       plot = plot_subscales_post,
       path = figurePath(),
       width = 8,
       height = 7,
       units = "cm",
       dpi = 1000,
       type = "cairo-png")



# Prepare data for item profile -------------------------------------------

## Add item texts
dat_tloc_long_summary_items <- 
  left_join(dat_tloc_long_summary,
            sett$quest$tloc$items$texts) %>% 
  mutate(subscales = sett$quest$tloc$subscales$col_names) %>% 
  arrange(desc(subscales)) %>% 
  mutate(item_text_fct = 
           factor(item_text, 
                  levels = item_text,
                  labels = breakStringToLines(item_text, max_nchar = 42) )) 



# Visualize item profile --------------------------------------------------

## Plot item profile
plot_profile <- 
  plotItemProfile(dat_tloc_long_summary_items,
                  col_name_items = "item_text_fct",
                  xlim = c(1.09, 16.91),
                  ylim = c(1, 5.2))

## Add errorbars on deepest level
plot_profile$layers <- 
  c(geom_errorbar(data = dat_tloc_long_summary_items,
                  aes_string(x = "item_text_fct",
                             ymin = "mean - ci",
                             ymax = "mean + ci"),
                  color = "grey50",
                  size = 0.3,
                  width = 0.2),
    plot_profile$layers)

## Add subscale information
plot_profile <- 
  plot_profile + 
  geom_rect(data = dat_tloc_long_summary_items,
            aes(xmin = as.numeric(item_text_fct) - 0.5, 
                xmax = as.numeric(item_text_fct) + 0.5, 
                ymin = sett$quest$tloc$scale$values_max + 0.2, 
                ymax = sett$quest$tloc$scale$values_max + 2,
                fill = subscales),
            alpha = 0.5) + 
  scale_fill_brewer(palette = "Set1")

plot(plot_profile)



# Post-process item profile -----------------------------------------------

plot_profile_post <- 
  plot_profile +
  
  ## Scale
  scale_color_manual(values = convRGB2Hex(47, 84, 150, 1)) + 
  scale_linetype_manual(values = "solid") +
  scale_shape_manual(values = 20) +
  scale_size_manual(values = 0.35) + 
  scale_alpha_manual(values = 0.35) + 
  
  ## Labels
  ggtitle(label = "Traffic Locus of Control",
          subtitle = paste("Ob ich in einen Autounfall verwickelt werde", 
"hängt ab von / davon ab, ...", sep = "\n")) + 
  labs(x = NULL,
       y = "Mittelwerte (95% KI)") +
  scale_x_discrete(position = "top") +
  guides(color = F, linetype = F, shape = F, size = F, alpha = F, fill = F) +
  
  ## Theme
  theme_thesis()

plot(plot_profile_post)

ggsave(filename = figureFileName("tloc_item-profile"), 
       plot = plot_profile_post,
       path = figurePath(),
       width = 8,
       height = 11,
       units = "cm",
       dpi = 1000,
       type = "cairo-png")
