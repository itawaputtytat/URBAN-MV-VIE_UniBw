# VISUALISATION -----------------------------------------------------------

source("fun_Liebner_2013/settings/set4plot.R")

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
  
  plotdat_prob <-
    ggplot(results,
           aes(x = rownames(results),
               y = I,
               fill = rownames(results))) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = paste(round(I, 1), "%")),
              vjust = 2,
              size = 3) +
    coord_cartesian(ylim = c(0,100)) +
    scale_fill_manual(name = "Intent",
                      labels = c("Going straight",
                                 "Stop at stop line",
                                 "Turn right",
                                 "Turn right but stop"),
                      values = c("cyan", "orange", "red", "magenta")) + 
    scale_x_discrete(labels = paste("I", c(1:4), sep = "")) +
    scale_y_continuous(expand = c(0, 0))
  
  if(set4plot$detailed) plot(plotdat_prob)
}



# Visualise complete results ----------------------------------------------

if(set4plot$show) {
  
  plotdat_passing.dvm.pos.prof <-
    plotdat_passing.dvm.pos.prof +
    labs(title = "Simulated driving behaviour corresponding to hypothesis Hi",
         subtitle = paste("Passing:", set4dat$passing, "(carried out at", set4sim$pos4carryout, "m)")) +
    coord_cartesian(ylim = c(0, 30)) + 
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