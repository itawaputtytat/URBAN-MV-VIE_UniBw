# Postprocess enriched speed profiles -------------------------------------

temp_title <- 
  paste(toupper(sett$meta$am_ger),
        "bei maximaler lateraler Beschleunigung")
temp_subtitle <- 
  paste("Abbiegerichtung:", 
        sub("abbiegen", "", sett$plot$title_maneuver))

plot_speed_acc_lon_post <- 
  plot_speed_acc_lon + 
  scale_y_continuous(breaks = seq(0, 100, 20)) +
  ggtitle(label = temp_title,
          subtitle = temp_subtitle) +
  labs(x = sett$plot$labels$x,
       y = "Geschwindigkeit (km/h)") +
  theme_thesis()

sett$plot$file_name <- 
  paste_("speed-profiles",
         sett$plot$file_name_prefix,
         sett$plot$file_name_suffix,
         sett$meta$am, "at_max_acc_lon")

ggsave(filename = figureFileName(sett$plot$file_name), 
       plot = plot_speed_acc_lon_post,
       path = figurePath(),
       width = 16,
       height = 5.5,
       units = "cm",
       dpi = 1000,
       type = "cairo-png")



# Visualize GPS of max. lat. acc. on map ----------------------------------

map <- get_map(location = c(11.641364, 48.075822), maptype = "satellite", zoom = 19)

ggmap(map) + 
  geom_point(data = dat_speed_acc_lon,
             aes(x = gps_lon,
                 y = gps_lat),
             color = "red")
