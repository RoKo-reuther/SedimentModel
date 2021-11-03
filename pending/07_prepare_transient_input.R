
###########################################################################################################
#               Input parameters to solve transient function: load-in of steady-state values              #
###########################################################################################################

extract_ss <- function(){
  # make new vecor to put steady state solution
  da_ss <- c()
  
  # Assign steady state solution values to new named vector as input for transient model
  # data has to be in one column for all state variables in same sequence as in "names_out" (= as in "species_operational" = as in first vector of "returnlist")
  
  for (specie in names_out){
    to_add <- ss$y[, specie]
    #names(to_add) <- rep(specie, N)
    da_ss <- c(da_ss, to_add)
  }
  da_ss <<- da_ss
}

extract_ss()
rm(extract_ss)