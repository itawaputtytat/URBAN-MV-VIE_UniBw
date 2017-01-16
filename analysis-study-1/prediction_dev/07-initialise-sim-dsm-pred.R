
# Initialise bayesian network ---------------------------------------------

## Initialise probability collector
P_O_Hi <- idm_createSimDat(
  list(j = length(set4bn$states$I),
       k = length(set4bn$states$V), 
       l = length(set4bn$states$A)), "", prefix = "")

P_O_Hi <- P_O_Hi[set4bn$idorder]

bn <- predLiebner_initBN("V1", set4bn)



# Load desired velocity models --------------------------------------------

## Load data
#dat4dsm <- read.csv(set4dsm$filepath, header = T)
dat4dsm <- 
  clustcentres %>% 
  group_by(clustgroup, sxx_dist_m_rnd1) %>% 
  summarise(speed_ms.u.limit.avg = mean(speed_ms.u.limit.avg)) %>% 
  #filter(round_txt == "intro") %>% 
  select_(k = "clustgroup", dist = "sxx_dist_m_rnd1",#set4dat$varname4dist_m, 
          speed = "speed_ms.u.limit.avg")
          #speed = "speed_ms.avg")

## Rename clustgroup to k1 to kx
dat4dsm$k <- paste("k", dat4dsm$k, sep = "")

## Spread column "values" into columns k1, k2 and k3
dat4dsm.spread <-
  dat4dsm %>% 
  spread(k, speed) #%>% 
#mutate(row = NULL)

# set4dsm$objname4dsm <- "dat4dsm.spread_v2"
# set4dsm$objname4dsm <- "dat4dsm.spread"

clustresults_means_h <- 
  read.csv2("../../URBAN-MV-VIE_UniBw_V3/modeling_Liebner_2013_V3/_archive/clustresults_means_h.txt",
            sep = ",",
            dec = ".",
            stringsAsFactors = F)

dat4dsm.spread_v2 <- c()
dat4dsm.spread_v2$dist <- as.numeric(unique(clustresults_means_h$temp_dist))
dat4dsm.spread_v2$k1 <- clustresults_means_h[which(clustresults_means_h$clust_nr == 1), "values"]
dat4dsm.spread_v2$k2 <- clustresults_means_h[which(clustresults_means_h$clust_nr == 2), "values"]
dat4dsm.spread_v2$k3 <- clustresults_means_h[which(clustresults_means_h$clust_nr == 3), "values"]
dat4dsm.spread_v2 <- as.data.frame(dat4dsm.spread_v2)
dat4dsm.spread_v2$dist <- round(dat4dsm.spread_v2$dist, 1)

print(set4dsm$objname4dsm)
dat4dsm <- 
  get(set4dsm$objname4dsm) %>% 
  data.frame()



