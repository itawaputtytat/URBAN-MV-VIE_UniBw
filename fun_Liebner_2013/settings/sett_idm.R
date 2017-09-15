## IDM
sett_idm <- c()
sett_idm$a     <- 0.5        ## Maximum acceleration a (m/s^2)
sett_idm$delta <- 4          ## Acceleration exponent delta
sett_idm$u_max <- 60 / 3.6   ## Desired velocity u: u = 0...60/km/h
sett_idm$b     <- 3          ## Comfortable deceleration b (m/s^2)
sett_idm$d0    <- 0          ## Minimum gap e.g. to leading vehicle d0 (m); d0 = 2; Set to zero, as there is no detection of preceding vehicles
sett_idm$T    <- 0.8           ## Time gap to leading vehicle T (s); T = 0.8


