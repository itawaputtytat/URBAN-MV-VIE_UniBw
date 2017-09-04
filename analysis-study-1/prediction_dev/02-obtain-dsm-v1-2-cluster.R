
# Preparatory settings ----------------------------------------------------

sett_proc$varname4speed <- "speed_ms.u.limit"
sett_proc$col_name_dist_m <- "pxx_dist_m_rnd1" 
#sett_proc$varname4speed <- "speed_ms.u.limit"
#sett_proc$varname4speed <- "speed_ms"
sett_proc$varname4round <- "round_txt"
sett_proc$k <- 3
sett_proc$procedure = "kmeans"
sett_proc$algorithm = "Hartigan-Wong"



# Prepare data for processing ---------------------------------------------

## Get data
#dat4clust <- t_adtf_dist_m_rnd1_full.intrpl.cut %>% data.frame()
# stoppings <-
#   t_adtf_dist_s_rnd1_full.intrpl.cut %>% 
#   select(passing, stopping) %>% 
#   group_by(passing) %>% 
#   summarise(stopping = min(stopping))

dat4clust <- dat4idm %>% data.frame()
# dat4clust <- 
#   left_join(dat4idm,
#             stoppings) %>% 
#   data.frame()

dat4clust <-
  dat4clust %>%
  filter(stopping == "no_stopping") %>%
  data.frame()

dat4clust <- 
  dat4clust %>% 
  filter_(paste( sett_proc$col_name_dist_m, ">= -50 & ", 
                 sett_proc$col_name_dist_m, "<= 25" )) 
 
## Select only necessary variables
dat4clust.clean <-
  dat4clust %>%
  select_(.dots = c(sett_proc$col_name_group, 
                    sett_proc$col_name_dist,
                    speed_val = sett_proc$varname4speed)) #%>%
# mutate(dist2sx_m_v2a_rnd1 = round(dist2sx_m_v2a_rnd1, 1)) %>%

## Create wide data format
dat4clust.clean.spread <-
  dat4clust.clean %>%
  ## Create new rows for each speed variable
  gather(speed_varname, speed_val, speed_val) %>%
  mutate(speed_varname = NULL) %>%
  spread_(sett_proc$col_name_dist , "speed_val")

## Clustering-algorithms require "clean" dataframes
## ... without additional variables
## ... so column passings has to be removed
## But: Information is backuped in rownames before deleting
rownames(dat4clust.clean.spread) <- 
  dat4clust.clean.spread[, sett_proc$col_name_group]
dat4clust.clean.spread[, sett_proc$col_name_group] <- NULL



# Run cluster algorithm ---------------------------------------------------

## In case of missing preceding or following values
## ... find section with complete data
## Necessary when clustering time-based-data
#min <- min(which(sapply(dat4clust.clean.spread.filtered, function(x) { sum(is.na(x)) }) == 0))
#max <- max(which(sapply(dat4clust.clean.spread.filtered, function(x) { sum(is.na(x)) }) == 0))

clustoutput <-
  #clust2groups(dat4clust.clean.spread.filtered[, min:max],
  clust2groups(dat4clust.clean.spread,
               k = sett_proc$k, 
               "kmeanspp", 
               algorithm = "Hartigan-Wong")

## Print assignments
print(clustoutput$assignment)



# Merge with data ---------------------------------------------------------

# Convert passing variable to character for mergind
clustoutput$assignment$passing <-
  as.character(clustoutput$assignment$passing)

## Join data and cluster assignment
dat4clust <- left_join(dat4clust, clustoutput$assignment) %>% data.frame()



# Visualize clustered desired velocity profiles ---------------------------

plotdat.clust <-
  ggplot(dat4clust) +
  geom_line(aes_string(x = sett_proc$col_name_dist_m,
                       y = sett_proc$varname4speed,
                       group = sett_proc$col_name_group,
                       colour = "clustgroup"),
            size = 1
            ,alpha = 0.35
            ) +
  geom_vline(xintercept = sett_synth$dist_limit, linetype = "dotted") +
  # stat_summary(aes_string(x = "dist2sx_m_v2a",
  #                         y = "speed_kmh"),
  #              geom = "line",
  #              fun.y = "mean",
  #              colour = "black",
  #              size = 1) +
  #facet_grid(.~round_txt) + 
  guides(colour = F) +
  coord_cartesian(xlim = c(-50, 25),
                  ylim = c(  0, 25)) + 
  scale_x_continuous(expand = c(0,0)) + 
  theme_bw()

#plot(plotdat.clust)



# Compute cluster centres -------------------------------------------------

clustcentres <-
  dat4clust %>%
  select_("clustgroup",
          speed_val = sett_proc$varname4speed,
          sett_proc$col_name_dist,
          sett_proc$varname4round) %>%
  mutate(clustgroup = as.numeric(clustgroup)) %>% 
  #group_by_("clustgroup", sett_proc$col_name_dist) %>%
  group_by_(.dots = lapply(c("clustgroup", sett_proc$col_name_dist
                             #, sett_proc$varname4round
                             ), as.symbol)) %>%
  ## Variable name is temporary, see next step
  summarise(speed_val.avg = mean(speed_val))

## Rename columns
colnames(clustcentres)[which(colnames(clustcentres) == "speed_val.avg")] <-
  paste(sett_proc$varname4speed, ".avg", sep = "")



# Visualise general (ungrouped) cluster centres ---------------------------

plotdat.clustcenters <-
  plotdat.clust +
  geom_line(data = clustcentres,
            aes_string(x = sett_proc$col_name_dist_m,
                       y = paste(sett_proc$varname4speed, ".avg", sep = ""),
                       colour = "as.factor(clustgroup)"),
            size = 2) #+
  #scale_colour_manual(values = c("green3", "blue3", "red3", "orange3"))

plot(plotdat.clustcenters)

# plot.centers <- ggplot() + 
#   geom_line(data = clust.centers,
#             aes_string(x = sett_proc$col_name_dist_m,
#                        y = paste(sett_proc$varname4speed, ".avg", sep = ""),
#                        linetype = "clustgroup", colour = sett_proc$varname4round),
#             size = 1) +
#   scale_colour_manual(values = c("green3", "blue3", "red3")) +
#   scale_linetype_manual(values = c("solid", "dashed", "dotted"))


# 
# # Compute cluster distribution for each round -----------------------------
# 
# datwclust.distr <- 
#   datwclust %>% 
#   filter(pxx_dist_m_rnd1 == 0) %>% 
#   group_by(round_txt, clustgroup) %>% 
#   summarise(count = n())
# 
# 
# 
# # Visualise cluster distribution ------------------------------------------
# 
# plot.clustdistr <-
#   ggplot() + 
#   geom_bar(data = datwclust.distr, 
#            aes(x = clustgroup, 
#                y = count, 
#                fill = clustgroup), 
#            stat = "identity") + 
#   facet_wrap(reformulate(sett_proc$varname4round)) + 
#   coord_cartesian(ylim = c(0, 50)) + 
#   #scale_fill_manual(values = c("green3", "blue3", "red3", "orange3")) + 
#   guides(fill = FALSE) +
#   theme_bw()
# 
# grid.arrange(plotdat.clustcenters, plot.clustdistr, nrow = 2)
#            