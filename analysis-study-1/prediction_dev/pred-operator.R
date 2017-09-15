
# Settings: Data ----------------------------------------------------------

sett_dat <- c()
sett_dat$df_name <- "study1_t_adtf_pxx_full_dti_rnd1_intrpld_cut"

sett_dat$case <- "p04_stress_s01"
#sett_dat$case <- unique(get(sett_dat$df_name)[, sett_dat$col_name_group])
sett_dat$am_limit1 <- -75
sett_dat$am_limit2 <- 25

sett_dat$col_name_am <- "pxx_dti_m_rnd1"
sett_dat$col_name_group <- "passing"
sett_dat$col_name_time <- "time_s"
sett_dat$col_name_dist <- "dist_m"
sett_dat$col_name_speed <- "speed_ms"
sett_dat$col_name_acc_lon <- "acc_lon_ms2"



# Settings: Procedure -----------------------------------------------------

sett_proc <- c()
sett_proc$stepwise_pause <- F
sett_proc$stepwise_proc_time <- F
sett_proc$plot = T
sett_proc$real_time = F
sett_proc$append_results = F 
sett_proc$carryout_am1 <- -75
sett_proc$carryout_am2 <- 20
sett_proc$carryout_step <- 1



# Settings: Simulation ----------------------------------------------------

## Initialise Algorithm, dsm and BN
source("fun_Liebner_2013/settings/sett_sim.R")
#sett_sim$objpos <- c(0, -0.1, 0, 7)
sett_sim$objpos[2] <- 0
sett_sim$objpos[4] <- 4
## Set position for carrying out simulation



# Settings: Visualization -------------------------------------------------

sett_vis <- c()
sett_vis$sim$xmin <- -75
sett_vis$sim$xmax <- 25
sett_vis$sim$ymin <- 0
sett_vis$sim$ymax <- 20
sett_vis$plot_simulation_history <- F



# Settings: Misc ----------------------------------------------------------

source("fun_Liebner_2013/settings/sett_bn.R")
#sett_bn$prior$V <- c(0.01, 0.98, 0.01)
source("fun_Liebner_2013/settings/sett_dsm.R")
sett_dsm$objname4dsm <- "dat_dsm.spread"
#sett_dsm$objname4dsm <- "dat4dsm.spread_v2"
source("fun_Liebner_2013/settings/sett_algo.R")
source("fun_Liebner_2013/settings/sett_idm.R")
#sett_idm$d0 <- 2
sett_idm$d0 <- 0



# Init --------------------------------------------------------------------

source("analysis-study-1/prediction_dev/initialise-sim-dsm-pred.R")
## Template for visualisation
#source("analysis-study-1/prediction_dev/zz-vis-classic-template_single.R")



# Run prediction for single position --------------------------------------

source("analysis-study-1/prediction_dev/load-test-data.R")
source("analysis-study-1/prediction_dev/xx-pred-single.R")



# Run prediction for group pf passings -------------------------------------

coll_overall <- c()
for(s in sett_dat$case) {
  sett_dat$case <- s
  outputString(paste("* Currently processing:", s))
  source("analysis-study-1/prediction_dev/load-test-data.R")
  source("analysis-study-1/prediction_dev/xx-pred-complete.R")
}



# Print evolution of prior ------------------------------------------------

coll_overall_avg <- 
  coll_overall %>% 
  group_by(pos4carryout) %>% 
  summarise_all(funs(mean(., na.rm = TRUE)))

graphics.off()
ggplot() + 
  geom_line(data = coll_overall,
            aes(x = pos4carryout,
                y = Intent1,
                group = passing),
            color = "#6FCDDD",
            alpha = 0.25) +
  geom_line(data = coll_overall_avg,
            aes(x = pos4carryout,
                y = Intent1),
            size = 2,
            color = "#6FCDDD") + 
  geom_line(data = coll_overall,
            aes(x = pos4carryout,
                y = Intent2,
                group = passing),
            color = "orange",
            alpha = 0.25) +
  geom_line(data = coll_overall_avg,
            aes(x = pos4carryout,
                y = Intent2),
            size = 2,
            color = "orange") + 
  geom_line(data = coll_overall,
            # %>% filter(passing == "p04_stress_s02"),
            aes(x = pos4carryout,
                y = Intent3,
                group = passing),
            color = "#ED2125",
            alpha = 0.25) +
  geom_line(data = coll_overall_avg,
            aes(x = pos4carryout,
                y = Intent3),
            size = 2,
            color = "#ED2125") + 
  # facet_grid(passing~.) + 
  # geom_line(data = coll_overall,
  #           aes(x = pos4carryout,
  #               y = Intent4,
  #               group = passing),
  #           color = "#B9539F",
  #           alpha = 0.5) +
  # geom_line(data = coll_overall_avg,
  #           aes(x = pos4carryout,
  #               y = Intent4),
  #           size = 2,
  #           color = "#B9539F") + 
  coord_cartesian(ylim = c(0, 1)) +
  scale_x_continuous(expand = c(0, 0)) +
  theme_bw()


# plot(x = coll4prior$s, y = coll4prior$X1, type = "l", col = "blue", ylim = c(0,1), main = "PriorDev")
# lines(x = coll4prior$s, coll4prior$X2, col = "orange")
# lines(x = coll4prior$s, coll4prior$X3, col = "red")
# lines(x = coll4prior$s, coll4prior$X4, col = "magenta")