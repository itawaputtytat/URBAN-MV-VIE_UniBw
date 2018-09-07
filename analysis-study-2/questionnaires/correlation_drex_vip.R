
# Settings ----------------------------------------------------------------

sett <- list()
sett$db$db_names$study2 <- "URBAN-MV-VIE_UniBw_Study-2"
sett$db$db_names$quest <- "Questionnaires"
sett$db$conn_names$study2 <- dbFindConnObj(sett$db$db_names$study2, output = F)
sett$db$conn_names$quest <- dbFindConnObj(sett$db$db_names$quest, output = F)
sett$db$src_names$drex <- "v_q_driving_experience"
sett$db$src_names$vip <- "v_q_vip_scores" 
sett$lang <- "ger"
sett$filter1 <- "driving_frequency_per_week"
sett$filter1_subscale <- sett$filter1
sett$filter2 <- "vip"
sett$file_name_prefix <- "drex"

initQuestSettings(sett$filter2, 
                  sett$db$conn_names$quest, 
                  "VIP_Schmidt_2012",
                  lang = sett$lang)

if (sett$filter1 == "driving_licence_years") {
  sett$filter1_name = "Länge des Führerscheinbesitzes"
}
if (sett$filter1 == "regular_driving_years") {
  sett$filter1_name = "Jahre regelmäßigen Fahrens" 
}
if (sett$filter1 == "km_overall") {
  sett$filter1_name = "Fahrleistung gesamt"
}
if (sett$filter1 == "km_recent_year") {
  sett$filter1_name = "Fahrleistung in den letzten 12 Monaten"
}
if (sett$filter1 == "driving_frequency_per_week") {
  sett$filter1_name = "Fahrhäufigkeit je Woche"
}



# Query data --------------------------------------------------------------

dat_drex <- dbGetSrc(sett$db$conn_names$study2, sett$db$src_names$drex)
dat_vip <- dbGetSrc(sett$db$conn_names$study2, sett$db$src_names$vip)



# Preprocess data ---------------------------------------------------------

## Correct driving licence year
dat_drex <- 
  dat_drex %>% 
  mutate(driving_licence_years = 2015 - driving_licence_year) %>% 
  ## Special case: Re-compute 
  mutate(driving_licence_years = 
           ifelse(subject_id == 19,
                  2015 - driving_licence_years,
                  driving_licence_years))

## Compute average driving frequence per year
dat_drex <- 
  dat_drex %>% 
  mutate(driving_frequency_per_week = 
           factor(driving_frequency_per_week,
                  labels = c(1.5, 3.5, 5.5, 7))) %>% 
  mutate(driving_frequency_per_week = 
           as.numeric(
             as.character(driving_frequency_per_week)
           )) 

## Select final
dat_drex <- 
  dat_drex %>% 
  select(subject_id,
         driving_licence_years,
         regular_driving_years,
         km_overall,
         km_recent_year,
         driving_frequency_per_week)

dat_vip <- 
  dat_vip %>% 
  select(subject_id,
         vip_ai = vip_ai_pr,
         vip_ed = vip_ed_pr,
         vip_sd = vip_sd_pr,
         vip_us = vip_us_pr)



# Join data ---------------------------------------------------------------

dat_all <- left_join(dat_drex, dat_vip)



# Correlation visualization -----------------------------------------------

pairs.panels(
  dat_all[, 2:ncol(dat_all)],
  method = "pearson",
  hist.col = "#00AFBB",
  density = TRUE,
  ellipses = TRUE,
  stars = TRUE
)



# Correlation matrix ------------------------------------------------------

dat_corr <- corr.test_v2(dat_all[,2:ncol(dat_all)])

## Create flat table
dat_corr_flat <- dat_corr$ci

## Get pair names
dat_corr_flat$pair_name <- rownames(dat_corr_flat)
rownames(dat_corr_flat) <- NULL

## Get seperate variable names
dat_corr_flat <- 
  dat_corr_flat %>% 
  rowwise() %>% 
  mutate(var1 = strsplit(pair_name, "-")[[1]][1],
         var2 = strsplit(pair_name, "-")[[1]][2]) %>% 
  data.frame()

## Round values
col_finder <- c("lower", "upper", "r")
dat_corr_flat[, col_finder] <- round(dat_corr_flat[, col_finder], 2)
col_finder <- c("p")
dat_corr_flat[, col_finder] <- round(dat_corr_flat[, col_finder], 3)



# Prepare data vor visualization ------------------------------------------

dat_plot <- dat_corr_flat

## Remember column with main values
if (length(unique(dat_plot$var1_subscale)) != 1) {
  col_finder_subscales_main <- "var1_subscales"
  col_finder_subscales_sec <- "var2_subscales"
} else {
  col_finder_subscales_main <- "var2_subscales"
  col_finder_subscales_sec <- "var1_subscales"
}

## Extract subscale
dat_plot <- 
  dat_plot %>% 
  rowwise() %>% 
  mutate(var1_subscales = var1,
         var2_subscales = strsplit(var2, "_")[[1]][2]) %>% 
  data.frame()

## Filter for questionnaires
dat_plot <- 
  dat_plot %>% 
  filter(grepl(sett$filter1, pair_name) & 
           grepl(sett$filter2, pair_name)) 

## Filter for specific subscale
row_finder <- which(dat_plot[col_finder_subscales_main] == sett$filter1_subscale)
dat_plot <- dat_plot[row_finder, ]

## Get scale names
dat_subscale_names <- sett$quest[[sett$filter2]]$subscales$abbr_eng_and_names_lang
names(dat_subscale_names) <- c(col_finder_subscales_sec, "subscale_name")
dat_subscale_names[, col_finder_subscales_sec] <- 
  tolower(dat_subscale_names[, col_finder_subscales_sec])
dat_plot <- 
  left_join(
    dat_plot,
    dat_subscale_names)

row_finder <- which(is.na(dat_plot[, "subscale_name"]))
dat_plot[row_finder, "subscale_name"] <- "vip gesamt"

## Sort values
dat_plot <- 
  dat_plot %>% 
  arrange(r)

## Save order of elements
levels <- 
  dat_plot %>% 
  distinct_(col_finder_subscales_sec) %>% 
  unlist(use.names = FALSE) %>% 
  rev()

labels <- 
  dat_plot[, "subscale_name"] %>% 
  rev()

## Create factor to sort elements in plot
dat_plot$subscales_fct <- 
  factor(dat_plot[, col_finder_subscales_sec], 
         levels = levels,
         labels = labels)

## Set colors
dat_plot <- 
  dat_plot %>% 
  mutate(color_r = 
           ifelse(r < 0,
                  RColorBrewer::brewer.pal(11, "RdYlGn")[2],
                  RColorBrewer::brewer.pal(11, "RdYlGn")[10] )) %>% 
  mutate(color_p = 
           ifelse(p < 0.05,
                  color_r,
                  "black" ))

## Set texts
dat_plot <- 
  dat_plot %>% 
  mutate(text_r = paste0("r = ", r)) %>% 
  mutate(text_p = paste0("p = ", p)) %>% 
  # mutate(text_position_y = ifelse(r < 0, 0.05, 0),
  #        text_position_y = ifelse(r > 0, -0.45, text_position_y))
  mutate(text_position_y = r + sign(r) * 0.08,
         text_position_y =
           ifelse(r < 0,
                  text_position_y - 0.35,
                  text_position_y))



# Visualization -----------------------------------------------------------

## Plot bars
plot_corr <- 
  ggplot() + 
  geom_bar(data = dat_plot,
           aes(x = subscales_fct ,
               y = r,
               fill = color_r ),
           stat = "identity",
           fill = dat_plot$color_r) + 
  coord_flip(ylim = c(-1, 1)) 


## Plot coefficients and p values for positive correlations
dat_plot_temp <- dat_plot %>% filter(r > 0)
plot_corr <- 
  plot_corr + 
  geom_text(data = dat_plot_temp,
            aes(x = subscales_fct,
                y = text_position_y,
                label = text_r),
            vjust = -0.3,
            hjust = 0,
            size = size,
            fontface = "bold",
            color = dat_plot_temp$color_p) + 
  geom_text(data = dat_plot_temp,
            aes(x = subscales_fct,
                y = text_position_y,
                label = text_p,
                color = color_p),
            vjust = 1.3,
            hjust = 0,
            size = size,
            color = dat_plot_temp$color_p)


## Plot coefficients and p values for negative correlations
dat_plot_temp <- dat_plot %>% filter(r < 0)
plot_corr <- 
  plot_corr + 
  geom_text(data = dat_plot_temp,
            aes(x = subscales_fct,
                y = text_position_y,
                label = text_r,
                color = color_p),
            vjust = -0.3,
            hjust = 0,
            size = size,
            fontface = "bold",
            color = dat_plot_temp$color_p) + 
  geom_text(data = dat_plot_temp,
            aes(x = subscales_fct,
                y = text_position_y,
                label = text_p,
                color = color_p),
            vjust = 1.3,
            hjust = 0,
            size = size,
            color = dat_plot_temp$color_p)


## Add separating line at r = 0
plot_corr <- 
  plot_corr + 
  geom_hline(aes(yintercept = 0),
             size = 0.1)

## Get name of main subscale
row_finder <- which(
  tolower(sett$quest[[sett$filter1]]$subscales$
            abbr_eng_and_names_lang$
            subscale_name_eng_abbr) == 
    sett$filter1_subscale)
temp <- 
  sett$quest[[sett$filter1]]$subscales$
  abbr_eng_and_names_lang[row_finder, "subscale_name"]
temp <- paste0("Fahrerfahrung: ", sett$filter1_name)

plot_corr_post <- 
  plot_corr +
  guides(color = F) + 
  scale_x_discrete(position = "top") +
  ggtitle("Korrelation von Fahrerfahrung und VIP",
          subtitle = temp) + 
  labs(x = NULL,
       y = "Korrelation") + 
  theme_thesis()

temp <- 
  paste_("correlation",
         sett$file_name_prefix,
         sett$filter1,
         sett$filter2)

ggsave(filename = figureFileName(temp),
       plot = plot_corr_post,
       path = figurePath(),
       width = 8,
       height = 4,
       units = "cm",
       dpi = 1000,
       type = "cairo-png")