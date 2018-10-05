
# Visualise simulated speed profiles --------------------------------------
dev.set(2)
source("prediction/resources_plot/_plot_simulation.R")


## Visualize simulated speed values
dev.set(3)
source("prediction/resources_plot/_plot_simulation_speed.R")


## Visualize simulated distance values
dev.set(4)
source("prediction/resources_plot/_plot_simulation_distance.R")

# Visualise current intent probabilites -----------------------------------

# dev.set(5)
# plot(bn)

dev.set(6)
source("prediction/resources_plot/_plot_current-prob.R")

