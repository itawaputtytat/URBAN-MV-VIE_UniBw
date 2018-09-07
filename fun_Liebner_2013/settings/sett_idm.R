
# Settings for IDM --------------------------------------------------------

## a:     Maximum acceleration a (m/s^2)
## delta: Acceleration exponent delta
## u_max: Desired velocity u: u = 0...60/km/h
## b:     Comfortable deceleration b (m/s^2)
## d0:    Minimum gap e.g. to leading vehicle d0 (m); d0 = 2 
## T:     Time gap to leading vehicle T (s); T = 0.8

sett_idm <- c()
sett_idm$a <- 0.5        
sett_idm$delta <- 4          
sett_idm$u_max <- 60 / 3.6   
sett_idm$b <- 3          
sett_idm$d0 <- 0 # Set to 0 as there is no detection of preceding vehicles
sett_idm$T <- 0.8           