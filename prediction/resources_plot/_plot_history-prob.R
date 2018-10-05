
if (!is.na(sett_sim_temp$am_1st)) {
  abline(v = sett_sim_temp$am_1st, col = "grey")
}
  
## Text for current position
rect(xleft = sett_plot$xmax -7, 
     xright = sett_plot$xmax -1,
     ybottom = 0.925, 
     ytop =  0.975,
     col = "white")
text(x = sett_plot$xmax - 4,
     y =  0.95,
     #labels = paste(round(sett_pred$carryout_am_start, 1), "m"))
     labels = paste(round(sett_sim_temp$am2, 1), "m"))

row_finder <-
  which(dat_pred_results_coll[, get(sett_case$col_names$am)] >=
          sett_sim_temp$am2_prev &
          dat_pred_results_coll[, get(sett_case$col_names$am)] <=
          sett_sim_temp$am2)

if (length(row_finder) == 0) {
  row_finder <- max(row_finder, 1)
}

for (i in sett_bn$state_names$I) {
  lines(dat_pred_results_coll[row_finder, get(sett_case$col_names$am)], 
        dat_pred_results_coll[row_finder, get(i)], 
        col = sett_plot$colors$prob[i],
        lwd = sett_plot$line_width$prob)
}
grid(NULL, NULL, lwd = 1, lty = "solid")
# testy <- dat_pred_results_coll[row_finder, ]
# 
# lines(testy[, get(sett_case$col_names$am)], 
#       testy$Intent1, 
#       col = "#6FCDDD",
#       lwd = 2)
# lines(testy[, get(sett_case$col_names$am)], 
#       testy$Intent2, 
#       col = "orange",
#       lwd = 2)
# lines(testy[, get(sett_case$col_names$am)], 
#       testy$Intent3, 
#       col = "#ED2125",
#       lwd = 2)
# lines(testy[, get(sett_case$col_names$am)], 
#       testy$Intent4, 
#       col = "#B9539F",
#       lwd = 2)

## Combine Intent3 and Intent4
# lines(testy[, get(sett_case$col_names$am)], 
#       testy$Intent3 + testy$Intent4, 
#       col = "#ED212550", 
#       lty = "dashed",
#       lwd = 2)


