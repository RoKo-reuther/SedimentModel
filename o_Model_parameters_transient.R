#=============================================================================
# Input parameters to solve transient function and load-in of steady-state values
#=============================================================================

#Solve transient model
#Time and time steps for solving transient model after steady state solution has been found
tmax <- 20    # number of years for run
tint <- 1/12      # time step [years]

#create time sequence
times <- seq(0, tmax, by = tint)

#make new data array to put steady state solution
da_ss <- array(data=NA, dim=c(length(ss$y)))

#Assign steady state solution values to new data array as input for transient model
for (bb in 1: length(names_out)) {
  da_ss[((bb-1)*N+1):(N*bb)] <- ss$y[1:N,bb]    #data has to be in one column for all state variables in same sequence as in "names"
}

