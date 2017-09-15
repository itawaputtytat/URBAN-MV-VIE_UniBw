
# Initialise bayesian network ---------------------------------------------

## Initialise probability collector
P_O_Hi <- idm_createSimDat(
  list(j = length(sett_bn$states$I),
       k = length(sett_bn$states$V), 
       l = length(sett_bn$states$A)), "", prefix = "")

P_O_Hi <- P_O_Hi[sett_bn$idorder]

bn <- predLiebner_initBN("V1", sett_bn)



# Load desired velocity models --------------------------------------------

## Load data
#dat_dsm <- read.csv(sett_dsm$filepath, header = T)
dat_dsm <- 
  dat_clust_centers %>% 
  group_by(cluster_group, pxx_dti_m_rnd1) %>% 
  summarise(speed_ms_u_smoothed_limitted_avg  = mean(speed_ms_u_smoothed_limitted_avg )) %>% 
  #filter(round_txt == "intro") %>% 
  select_(k = "cluster_group", dist = "pxx_dti_m_rnd1",#sett_dat$varname4dist_m, 
          speed = "speed_ms_u_smoothed_limitted_avg ")
          #speed = "speed_ms.avg")

## Rename cluster_group to k1 to kx
dat_dsm$k <- paste("k", dat_dsm$k, sep = "")

## Spread column "values" into columns k1, k2 and k3
dat_dsm.spread <-
  dat_dsm %>% 
  spread(k, speed) #%>% 
#mutate(row = NULL)

# sett_dsm$objname4dsm <- "dat_dsm.spread_v2"
# sett_dsm$objname4dsm <- "dat_dsm.spread"

clustresults_means_h <- 
  read.csv2("../../URBAN-MV-VIE_UniBw_V3/modeling_Liebner_2013_V3/_archive/clustresults_means_h.txt",
            sep = ",",
            dec = ".",
            stringsAsFactors = F)

dat_dsm.spread_v2 <- c()
dat_dsm.spread_v2$dist <- as.numeric(unique(clustresults_means_h$temp_dist))
dat_dsm.spread_v2$k1 <- clustresults_means_h[which(clustresults_means_h$clust_nr == 1), "values"]
dat_dsm.spread_v2$k2 <- clustresults_means_h[which(clustresults_means_h$clust_nr == 2), "values"]
dat_dsm.spread_v2$k3 <- clustresults_means_h[which(clustresults_means_h$clust_nr == 3), "values"]
dat_dsm.spread_v2 <- as.data.frame(dat_dsm.spread_v2)
dat_dsm.spread_v2$dist <- round(dat_dsm.spread_v2$dist, 1)

print(sett_dsm$objname4dsm)
dat_dsm <- 
  get(sett_dsm$objname4dsm) %>% 
  data.frame()



