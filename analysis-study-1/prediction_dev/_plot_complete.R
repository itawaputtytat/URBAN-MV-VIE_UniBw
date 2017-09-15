
## Visualise simulated speed profiles
dev.set(3)
source("analysis-study-1/prediction_dev/_plot_simulation.R")


## Visualise current intent probabilites
dev.set(4)
source("analysis-study-1/prediction_dev/_plot_current-prob.R")


## Visualise history of intent probabilities
dev.set(5)
source("analysis-study-1/prediction_dev/_plot_history-prob.R")


## Visualize simulated speed values
dev.set(6)
source("analysis-study-1/prediction_dev/_plot_simulation_speed.R")


## Visualize simulated distance values
dev.set(7)
source("analysis-study-1/prediction_dev/_plot_simulation_distance.R")


## Dummy window to avoid flickering
## Must be called as last device
dev.set(2)