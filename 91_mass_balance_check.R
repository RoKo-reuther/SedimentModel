
###########################################################################################################
#                                            MASS BALANCE CHECKS                                          #
###########################################################################################################

library(stringr)

#*************************************
# define functions: steady state
#*************************************

# mass balance check for steady state results
mass_balance_steady <- function(specie, ss_results = ss, layer_depth = grid_collection$grid$dx, por = grid_collection$por.grid$mid, svf = grid_collection$svf.grid$mid){
  # mass balance: depth integrated "species-reaction-rate" + flux upwards (positive = into model domain) - flux downwards (positive = out of model domain) [mol/(m^2*y)]
  # performs a simple depth-integration through adding up for reaction rates; no interpolation
  
  # get species reaction rate depth profile
  rr_depth_p <- ss_results[[paste("R", specie, sep="")]]
  
  # check for species phase and set "volume-fraction conversion factor" (vfcf); converts 1/V_sf or 1/V_pw to 1/V_total
  if (species_operational[[specie]]$phase == "solute"){
    vfcf <- por
    } else {
    vfcf <- svf
  }
  
  # get depth integrated "species-reaction-rate": sum up reaction rates depth profile and convert from [mol/(V_sf*y)] resp. [mol/(V_pw*y)] to [mol/(m^2*y)]
  int_rr <- sum(rr_depth_p * vfcf * layer_depth) # layer_depth normalizes from volume to area
  
  # get fluxes
    # flux upwards
    flux_up <- ss_results[[paste("FU_", specie, sep="")]]
    # flux downwards
    flux_down <- ss_results[[paste("FD_", specie, sep="")]]
    
  # mass balance
  result <- int_rr + flux_up - flux_down
  return(result)
}

# create steady-state mass balance data frame
mass_balance_steady_all <- function(){
  ss_mass_balances <- data.frame(
    specie = c(),
    mass_balance = c()
  )
  for (specie in species_operational){
    new_data <- data.frame(
      specie = specie$name,
      mass_balance = mass_balance_steady(specie = specie$name)
    )
    ss_mass_balances <- rbind(ss_mass_balances, new_data)
  }
  ss_mass_balances <<- ss_mass_balances
}

#*************************************
# calculate steady state mass balance
#*************************************

# steady-state mass balance: single call
mass_balance_steady("FeS2")

# steady-state mass balance: all species
mass_balance_steady_all()


#*************************************
# define functions: transient
#*************************************

# mass balance check for transient results
mass_balance_transient <- function(specie, timesteps = parameters$times, trans_results = trans, layer_depth = grid_collection$grid$dx, por = grid_collection$por.grid$mid, svf = grid_collection$svf.grid$mid){
  # mass balance: depth integrated "species-reaction-rate" + flux upwards (positive = into model domain) - flux downwards (positive = out of model domain) [mol/(m^2*y)]
  # performs a simple depth-integration through adding up for reaction rates; no interpolation
  # calculates mass balance for one specie and all timesteps
  
  # prepare result vector
  result <- c()
  
  # check for species phase and set "volume-fraction conversion factor" (vfcf); converts 1/V_sf or 1/V_pw to 1/V_total
  if (species_operational[[specie]]$phase == "solute"){
    vfcf <- por
  } else {
    vfcf <- svf
  }
  
  for (i in seq_along(timesteps)){
    # get species reaction rate depth profile; rates for different layers are in different columns; one row is one timestep
    # prepare empty vector
    rr_depth_p <- c()
    # prepare "needle"; basically in attributes(trans)$dimnames[[2]] will be searched for the specie reaction term columns
    # to be sure to get only the right columns we search for e.g. "RO21", the first specie reaction rate column and take this and the next N-1 columns
    needle <- paste("R", specie, "1", sep="")
    # get start column
    startc <- which(attributes(trans)$dimnames[[2]] == needle)
    for (j in seq(from = 0, to = parameters$N-1)){
      rr_depth_p <- c(rr_depth_p, trans[i, startc+j])
    }
    
    # get depth integrated "species-reaction-rate": sum up reaction rates depth profile and convert from [mol/(V_sf*y)] resp. [mol/(V_pw*y)] to [mol/(m^2*y)]
    int_rr <- sum(rr_depth_p * vfcf * layer_depth) # layer_depth normalizes from volume to area
    
    # get fluxes
    # flux upwards
    flux_up <- trans_results[[i, paste("FU_", specie, sep="")]]
    # flux downwards
    flux_down <- trans_results[[i, paste("FD_", specie, sep="")]]
    
    # add mass balance for this timestep
    result <- c(result, (int_rr + flux_up - flux_down))
  }
  return(result)
}

# create detailed transient mass balance data frame
mass_balance_transient_all <- function(){
  trans_mass_balances <- data.frame(
    time = parameters$times
  )
  for (specie in species_operational){
    # add mass balances for one species for every timesteps as new row
    trans_mass_balances <- cbind(trans_mass_balances, mass_balance_transient(specie$name))
    # name new column
    attributes(trans_mass_balances)$names[length(trans_mass_balances)] <- specie$name
  }
  trans_mass_balances <<- trans_mass_balances
}

# create transient mass balance overview
mass_balance_transient_overview <- function(input=trans_mass_balances){
  # new data frame
  trans_mass_b_ov <- data.frame(
    value = c("sum", "sum of absolute values", "average [mol/(m^2*y)]")
  )
  # add named column for each specie
  for (i in seq(from = 2, to = length(input))){
    # new temporary data frame
    new_data <- data.frame(
      c = c(sum(trans_mass_balances[, i]), sum(abs(trans_mass_balances[, i])), mean(trans_mass_balances[, i]))
    )
    # name column
    attributes(new_data)$names <- names(trans_mass_balances)[i]
    # add new data to trans_mass_b_ov
    trans_mass_b_ov <- cbind(trans_mass_b_ov, new_data)
  }
  trans_mass_b_ov <<- trans_mass_b_ov
}

#*************************************
# calculate transient mass balance
#*************************************

# transient mass balance: single call
mass_balance_transient("OrgCB")

# transient mass balance: all species
mass_balance_transient_all()

# transient mass balance: overview
mass_balance_transient_overview()
