library(mvbutils)
oldpar <- par()
set.seed(42)
foodweb(where = list(1, "package:puttytat4R"),
        #prune = "outputFunProc",
        border = T,
        #expand.xbox = 1.1, expand.ybox = 1.6,
        boxcolor = "grey90",
        cex = 0.9,
        lwd = 2,
        use.centres = F)
par(oldpar)
dev.off()

#test <- foodweb(where = "package:puttytat4R", cex = 0.9, lwd = 2)
#FuncMap(test, pkg = "puttytat4R", method = "abs", sm.title = T)
