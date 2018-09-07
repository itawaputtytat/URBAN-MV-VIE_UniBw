readxl
zoo ## Interpolation, rollmean
tripack ## Compute path curvature
geosphere ## Computing gps-distances
#TraMineR # Sequence analysis
#TraMineRextras # Sequence analysis
#lazyeval # Dynamic filter variables in dplyr
## Cluster analysis
LICORS ## kmeanspp
dtw ## Hierarchical clustering distance measures
TSclust # Calculating different distances measures
#kml ## Clustering of longitudinal data
psych ## Correlation matrix (e.g. pairs.panels
lazyeval ## for interp
mice ## imputation


for(x in test) { 
  pos <- max(0, gregexpr("#", x)[[1]][1])
  
  if (pos == -1) {
    pos = nchar(x) + 1
  }
  
  res <- substr(x, 1, pos-1)
  res <- sub(" ", "", res)
  
  if (res != "") {
    library(res, character.only = T)
  }
  }
