
###########################################################################################################
#                             TINKER FUNCTIONS TO DESCRIBE VARYING CONDITIONS                             #
###########################################################################################################


#*************************************
# FeOH3A addition
#*************************************

### tinker function
myfunc <- function(p){
  # timesteps
  x <- c(1/12, 1.1/12, 1.9/12, 2/12)
  # related conditions (fluxes at upper boundary)
  y <- c(0, p, 2, 0)
  # get fitted function
  test <- splinefun(x, y)
    # draw function and fixed points
    curve(test, x[1], x[length(x)])
    points(x, y)
  # set condition to be minimized
  integrate(f=test, lower=x[1], upper=x[length(x)])$value - 1.8
}

optimize(f=myfunc, interval=c(21.000, 21.001))

### formulate fitted function 
  x <- c(1/12, 1.1/12, 1.9/12, 2/12)
  y <- c(0, 21.00005, 2, 0)
  Fe_addition <<- splinefun(x, y)

### check inegral: added iron
integrate(Fe_addition, 1/12, 2/12)
