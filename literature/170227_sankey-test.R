library(readxl)
litdat <- read_excel("D:/Dissertation/Literatur-Review_Prediction/170227_Literatur_cop.xls")
litdat.list <- split(litdat, seq(nrow(litdat)))



litdat.test <- litdat[c(1:5), ]
litdat.test$rnr <- 1:nrow(litdat.test)
litdat.test <- litdat.test[, c("rnr", "Author, editor or organization", "Year derived", "Categories")]
colnames(litdat.test) <- c("rnr", "author", "year", "tag")
library(splitstackshape)
litdat.test.split <- cSplit(melt(litdat.test, id.vars = c("rnr", "author", "year")), "value", ";", "long")
litdat.test.split <-
  litdat.test.split %>% 
  group_by(rnr) %>% 
  mutate(rnr_tag = row_number()) %>% 
  mutate(variable2 = 1) %>% 
  mutate(value = gsub("_", "", value)) %>% 
  mutate(value = gsub("-", "", value)) %>% 
  data.frame()

litdat.test.split.4plot <- 
  litdat.test.split %>% 
  select(rnr, year, value) %>% 
  mutate(rnr = as.character(rnr))
  
litdat.test.split.4plot <- 
  gather(litdat.test.split.4plot, "tag", "value", 2:3)

litdat.test.split.4plot.part1 <- 
  litdat.test.split.4plot %>% 
  filter(tag == "value") %>% 
  select(rnr, value)

litdat.test.split.4plot.part2 <- 
  litdat.test.split.4plot %>% 
  filter(tag == "year") %>% 
  select(value, rnr)

litdat.test.split.4plot.final <- 
  rbindlist(list(litdat.test.split.4plot.part2,
                 litdat.test.split.4plot.part1)) %>% 
  data.frame()
names(litdat.test.split.4plot.final) <- c("from", "to")

litdat.test.split.4plot.final$weight <- 1

Sankey <- 
  gvisSankey(litdat.test.split.4plot.final, 
           from = "from", 
           to = "to",
           weight = "weight")
plot(Sankey)



# Test data ---------------------------------------------------------------



# GoogleVis ---------------------------------------------------------------

library(readxl)
litdat <- read_excel("D:/Dissertation/Literatur-Review_Prediction/170227_POC_testdata_V2.xlsx")

library(googleVis)

Sankey <- 
  gvisSankey(litdat, 
             from = "from", 
             to = "to",
             weight = "weight",
             options = list(
               width = "30cm", 
               height = "10cm",
               Sankey = "{ iterations: 2 }"
             )
            )
plot(Sankey)



# rCharts -----------------------------------------------------------------

litdat[which(litdat[,1]==litdat[,2]),]

colnames(litdat) <- c("source", "target", "value")

sankeyPlot2 <- rCharts$new()
sankeyPlot2$setLib('http://timelyportfolio.github.io/rCharts_d3_sankey')
sankeyPlot2$set(
  data = litdat,
  nodeWidth = 15,
  nodePadding = 10,
  layout = 32,
  width = 960,
  height = 500
)
sankeyPlot2



devtools::install_github("ramnathv/rCharts")
library(rCharts)
library(readxl)
litdat <- read_excel("D:/Dissertation/Literatur-Review_Prediction/170227_POC_testdata.xlsx")
litdat <- read_excel("D:/Dissertation/Literatur-Review_Prediction/170227_POC_testdata_V2.xlsx")
colnames(litdat) <- c("source", "target", "value")

library(dplyr)
# litdat <- 
#   litdat %>% 
#   arrange(target)

sankeyPlot2 <- rCharts$new()
sankeyPlot2$setLib('http://timelyportfolio.github.io/rCharts_d3_sankey')
sankeyPlot2$set(
  data = litdat,
  nodeWidth = 15,
  nodePadding = 10,
  layout = 32,
  width = 960,
  height = 500
)
sankeyPlot2



# networkD3 ---------------------------------------------------------------

library(networkD3)
library(dplyr)
library(readxl)

litdat <- 
  #read_excel("D:/Dissertation/Literatur-Review_Prediction/170227_POC_testdata.xlsx") %>% 
  read_excel("D:/Dissertation/Literatur-Review_Prediction/170227_POC_testdata_V2.xlsx") %>% 
  data.frame() %>% 
  rename(source = from, target = to, value = weight) %>% 
  mutate(value = value * 100)
nodes <- unique(c(litdat$source, litdat$target))
nodes <- data.frame(name = nodes, stringsAsFactors = F)
#nodes$group <- c(rep("author", 15), rep("algo", 20-16+1), rep("man", 24-21+1))

colorset <- nodes
colorset$range <- 0
colorset$range[1:15] <- "#c8e3f8"
colorset$range[16:20] <- "#76ee00"
colorset$range[21:24] <- "#76ee01"
colorset$domain <- colorset$name

#litdat$group <- c(rep("man", 20), rep("algo", 39-21+1))

litdat2 <- litdat
litdat2$group <- litdat2$source
#litdat2$group <- 0
#litdat2$group[1:20] <- litdat2$target[1:20]
#litdat2$group[21:39] <- litdat2$source[21:39]

for(i in 1:nrow(litdat)) {
  litdat2$source[i] <- which(litdat2$source[i] == nodes) - 1
  litdat2$target[i] <- which(litdat2$target[i] == nodes) - 1
}
litdat2$source <- as.numeric(litdat2$source)
litdat2$target <- as.numeric(litdat2$target)
litdat2$value <- 100

networkD3::sankeyNetwork(Links = litdat2, 
              Nodes = nodes,
              Source = "source",
              Target = "target", 
              Value = "value", 
              NodeID = "name",
              NodeGroup = "name",
              LinkGroup = "source",
              colourScale = JS(
                sprintf(
                  'd3.scale.ordinal()
                  .domain(%s)
                  .range(%s)
                  ',
                  jsonlite::toJSON(colorset$domain),
                  jsonlite::toJSON(colorset$range)
                )),
              fontSize = 12, 
              nodeWidth = 30,
              fontFamily = "Consolas",
              iterations = 0)




# sankeyD3 ----------------------------------------------------------------

litdat <- 
  #read_excel("D:/Dissertation/Literatur-Review_Prediction/170227_POC_testdata.xlsx") %>% 
  read_excel("D:/Dissertation/Literatur-Review_Prediction/170227_POC_testdata_V2.xlsx") %>% 
  data.frame() %>% 
  rename(source = from, target = to, value = weight) %>% 
  mutate(value = value * 100)
nodes <- unique(c(litdat$source, litdat$target))
nodes <- data.frame(name = nodes, stringsAsFactors = F)
#nodes$group <- c(rep("author", 15), rep("algo", 20-16+1), rep("man", 24-21+1))

sankeyNetwork(Links = litdat2, 
              Nodes = nodes,
              Source = "source",
              Target = "target", 
              Value = "value", 
              NodeID = "name",
              NodeGroup = "name",
              LinkGroup = "target",
              fontSize = 20,
              fontFamily = "Calibri",
              iterations = 0,
              linkGradient = T,
              highlightChildLinks = F,
              doubleclickTogglesChildren = T,
              showNodeValues = F,
              linkOpacity = 0.25,
              nodePadding = 15,
              nodeLabelMargin = 5,
              nodeWidth = 25,
              orderByPath = F)
