# VISUALISATION -----------------------------------------------------------

source("fun_Liebner_2013/settings/set4plot.R")
set4sim$pos4carryout <- pos4carryout

# Visualise current case --------------------------------------------------

if(set4plot$show) 
  plotdat_passing <- 
    predLiebner_visPassing(dat4test, set4dat, set4plot$detailed)



# Visualise desired velocity models ---------------------------------------

if(set4plot$show) 
  plotdat_passing.dvm <- 
    predLiebner_visDVM(plotdat_passing, 
                       dat4dvm.spread, set4plot$detailed)

# Visualise parameter positions -------------------------------------------

if(set4plot$show)
  plotdat_passing.dvm.pos <- 
    predLiebner_visPos(plotdat_passing.dvm, 
                       set4sim, set4sim_temp, dat4sim, set4plot$detailed)



# Visualise predicted trajectories ----------------------------------------

if(set4plot$show) 
  plotdat_passing.dvm.pos.prof <- 
    predLiebner_visProf(plotdat_passing.dvm.pos, 
                        set4sim, set4dat, dat4sim, set4plot$detailed)



# Visualise intent probabilites -------------------------------------------

if(set4plot$show) {
  
  results4plot <- round(results * 100, 0)
  results4plot <- sprintf("%02g", as.vector((unlist(results4plot))))
  
  plotdat_prob <-
    ggplot(results4output,
           aes(x = rownames(results4output),
               y = P_O_Hi,
               fill = rownames(results4output))) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = paste(round(P_O_Hi, 1))),
              vjust = 2,
              size = 3) +
    coord_cartesian(ylim = c(0, 1)) +
    scale_fill_manual(name = "Intent",
                      # labels = c("Going straight",
                      #            "Stop at stop line",
                      #            "Turn right",
                      #            "Turn right but stop"),
                      labels = c(paste(results4plot[1], "% Going straight"),
                                 paste(results4plot[2], "% Stop at stop line"),
                                 paste(results4plot[3], "% Turn right"),
                                 paste(results4plot[4], "% Turn right but stop")),
                      #values = c("cyan", "orange", "red", "magenta")) + 
                      values = c("#6FCDDD", "orange", "#ED2125", "#B9539F")) + 
    scale_x_discrete(labels = paste("I", c(1:4), sep = "")) +
    scale_y_continuous(expand = c(0, 0))
  
  if(set4plot$detailed) plot(plotdat_prob)
}



# Visualise complete results ----------------------------------------------

if(set4plot$show) {
  
  plotdat_passing.dvm.pos.prof <-
    plotdat_passing.dvm.pos.prof +
    labs(x = "Distance-to-intersection (m)",
         y = "Speed (m/s)",
         title = "Simulated driving behaviour corresponding to hypothesis Hi",
         subtitle = paste("Passing:", set4dat$passing, "(carried out at", set4sim$pos4carryout, "m)")) +
    coord_cartesian(xlim = c(-50, 25),
                    ylim = c(0, 30)) + 
    theme_bw()
  
  plotdat_prob <-
    plotdat_prob +
    labs(x = "Intent",
         y = "Probability",
         title = "Probability P(Hi|v(t),s(t))",
         subtitle = ""
         # subtitle = paste("Sigma v = ", round(dat4prob$sigma_v_ms, 1), "and", 
         #                  "Sigma s = ", round(dat4prob$sigma_s_m, 1))
         ) + 
    theme_bw() + 
    theme(legend.background = element_rect(fill = "grey92"),
          #legend.position = c(0.01, 0.99),
          #legend.justification = c(0, 1),
          legend.position = "top",
          legend.justification = "left",
          legend.title = element_text(face = "bold")) +
    guides(fill = guide_legend(nrow = 4, byrow = TRUE,
                               title.position = "top"))
  
  grid.arrange(plotdat_passing.dvm.pos.prof,
               plotdat_prob,
               ncol = 2,
               widths = c(5,2))
}