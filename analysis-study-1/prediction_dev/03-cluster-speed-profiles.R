
# Preparatory settings ----------------------------------------------------

set4proc$varname4speed <- "speed_ms.u.limit"
set4proc$varname4round <- "round_txt"
set4proc$k <- 3
set4proc$procedure = "kmeans"
set4proc$algorithm = "Hartigan-Wong"



# Prepare data for processing ---------------------------------------------

## Get data
#dat4clust <- t_adtf_dist_m_rnd1_full.intrpl %>% data.frame()

dat4clust <- dat4idm %>% data.frame()

# dat4clust <-
#   dat4clust %>%
#   filter(stopping == "no_stopping") %>%
#   data.frame()

dat4clust <- 
  dat4clust %>% 
  filter_(paste( set4proc$varname4dist_m, ">= -50 & ", 
                 set4proc$varname4dist_m, "<= 25" )) 
 
## Select only necessary variables
dat4clust.clean <-
  dat4clust %>%
  select_(.dots = c(set4proc$varname4group, 
                    set4proc$varname4dist,
                    speed_val = set4proc$varname4speed)) #%>%
# mutate(dist2sx_m_v2a_rnd1 = round(dist2sx_m_v2a_rnd1, 1)) %>%

## Create wide data format
dat4clust.clean.spread <-
  dat4clust.clean %>%
  ## Create new rows for each speed variable
  gather(speed_varname, speed_val, speed_val) %>%
  mutate(speed_varname = NULL) %>%
  spread_(set4proc$varname4dist , "speed_val")

## Clustering-algorithms require "clean" dataframes
## ... without additional variables
## ... so column passings has to be removed
## But: Information is backuped in rownames before deleting
rownames(dat4clust.clean.spread) <- 
  dat4clust.clean.spread[, set4proc$varname4group]
dat4clust.clean.spread[, set4proc$varname4group] <- NULL



# Run cluster algorithm ---------------------------------------------------

## In case of missing preceding or following values
## ... find section with complete data
## Necessary when clustering time-based-data
#min <- min(which(sapply(dat4clust.clean.spread.filtered, function(x) { sum(is.na(x)) }) == 0))
#max <- max(which(sapply(dat4clust.clean.spread.filtered, function(x) { sum(is.na(x)) }) == 0))

clustoutput <-
  #clust2groups(dat4clust.clean.spread.filtered[, min:max],
  clust2groups(dat4clust.clean.spread,
               k = set4proc$k, 
               "kmeanspp", 
               algorithm = "Hartigan-Wong")

## Print assignments
print(clustoutput$assignment)



# Merge with data ---------------------------------------------------------

# Convert passing variable to character for mergind
clustoutput$assignment$passing <-
  as.character(clustoutput$assignment$passing)

## Join data and cluster assignment
datwclust <- left_join(dat4clust, clustoutput$assignment) %>% data.frame()



# Visualize clustered desired velocity profiles ---------------------------

plotdat.clust <-
  ggplot(datwclust) +
  geom_line(aes_string(x = set4proc$varname4dist_m,
                       y = set4proc$varname4speed,
                       group = set4proc$varname4group,
                       colour = "clustgroup"),
            size = 1
            ,alpha = 0.35
            ) +
  geom_vline(xintercept = set4synth$distlimit, linetype = "dotted") +
  # stat_summary(aes_string(x = "dist2sx_m_v2a",
  #                         y = "speed_kmh"),
  #              geom = "line",
  #              fun.y = "mean",
  #              colour = "black",
  #              size = 1) +
  facet_grid(.~round_txt) + 
  guides(colour = F) +
  coord_cartesian(xlim = c(-50, 25),
                  ylim = c(  0, 25)) + 
  scale_x_continuous(expand = c(0,0)) + 
  theme_bw()

# plot(plotdat.clust)



# Compute cluster centres -------------------------------------------------

clustcentres <-
  datwclust %>%
  select_("clustgroup",
          speed_val = set4proc$varname4speed,
          set4proc$varname4dist,
          set4proc$varname4round) %>%
  #group_by_("clustgroup", set4proc$varname4dist) %>%
  group_by_("clustgroup", set4proc$varname4dist, set4proc$varname4round) %>%
  ## Variable name is temporary, see next step
  summarise(speed_val.avg = mean(speed_val))

## Rename columns
colnames(clustcentres)[which(colnames(clustcentres) == "speed_val.avg")] <-
  paste(set4proc$varname4speed, ".avg", sep = "")



# Visualise cluster centres -----------------------------------------------

plotdat.clustcenters <-
  plotdat.clust +
  geom_line(data = clustcentres,
            aes_string(x = set4proc$varname4dist_m,
                       y = paste(set4proc$varname4speed, ".avg", sep = ""),
                       colour = "clustgroup"),
            size = 2) #+
  #scale_colour_manual(values = c("green3", "blue3", "red3", "orange3"))

#plot(plotdat.clustcenters)

# plot.centers <- ggplot() + 
#   geom_line(data = clust.centers,
#             aes_string(x = set4proc$varname4dist_m,
#                        y = paste(set4proc$varname4speed, ".avg", sep = ""),
#                        linetype = "clustgroup", colour = set4proc$varname4round),
#             size = 1) +
#   scale_colour_manual(values = c("green3", "blue3", "red3")) +
#   scale_linetype_manual(values = c("solid", "dashed", "dotted"))



# Compute cluster distribution for each round -----------------------------

datwclust.distr <- 
  datwclust %>% 
  filter(sxx_dist_m_rnd1 == 0) %>% 
  group_by(round_txt, clustgroup) %>% 
  summarise(count = n())



# Visualise cluster distribution ------------------------------------------

plot.clustdistr <-
  ggplot() + 
  geom_bar(data = datwclust.distr, 
           aes(x = clustgroup, 
               y = count, 
               fill = clustgroup), 
           stat = "identity") + 
  facet_wrap(reformulate(set4proc$varname4round)) + 
  coord_cartesian(ylim = c(0, 50)) + 
  #scale_fill_manual(values = c("green3", "blue3", "red3", "orange3")) + 
  guides(fill = FALSE) +
  theme_bw()

grid.arrange(plotdat.clustcenters, plot.clustdistr, nrow = 2)
           