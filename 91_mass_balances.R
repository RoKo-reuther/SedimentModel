
###########################################################################################################
#                                            MASS BALANCE CHECKS                                          #
###########################################################################################################

#*************************************
# steady state
#*************************************

### RX based mass balance
mass_balance_steady <- function(specie, ss_results = ss, layer_depth = grid_collection$grid$dx, por = grid_collection$por.grid$mid, svf = grid_collection$svf.grid$mid){
  # mass balance: depth integrated "species-reaction-rate" + flux upwards (positive = into model domain) - flux downwards (positive = out of model domain) [mol/(m^2*y)]
  # performs a simple depth-integration through adding up for reaction rates; no interpolation
  
  # get species reaction rate depth profile
  rr_depth_p <- ss_results[[paste("R", specie, sep="")]]
  
  # check for species phase and set "volume-fraction conversion factor" (vfcf); converts 1/V_sf or 1/V_pw to 1/V_total
  if (species_operational[[specie]]$phase == "solute"){
    vfcf <- grid_collection$por.grid$mid
  } else {
    vfcf <- grid_collection$svf.grid$mid
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


### create steady-state mass balance data frame
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
# dynamic scenario
#*************************************

# ### RX based mass balance
# mass_balance_transient <- function(specie, a=parameters$times[1], b=parameters$times[length(parameters$times)]){
#   # a: startpoint of mass balance (has to be a timestep in parameters$times)
#   # b: endpoint of mass balance (has to be a timestep in parameters$times)
#   
#   # get position of a and b in parameters$times
#   a <- which(round(parameters$times, digits = 15) == round(a, digits = 15))
#   b <- which(round(parameters$times, digits = 15) == round(b, digits = 15))
#   
#   # prepare dC pools
#   d_int_dCs <- c()  # depth integrated change in concentration through chemical reactions (RX) + through transport processes (trandC)         
#   
#   # a) integrate dC=RX+trandC over depth and time
#   # b) integrate fluxes at upper and lower boudnary from a to b
#   # c1) c2) add results to pools
#   
#   # check for species phase
#   if (species_operational[[specie]]$phase == "solute"){
#     phase <- "solute"
#   } else {
#     phase <- "solid"
#   }
# 
#   # a1) depth integration  
#   for (i in seq_along(parameters$times)){
#     
#     # get species reaction rate depth profile; rates for different layers are in different columns; one row is one timestep
#       # prepare empty vector
#       rr_depth_p <- c()
#       # prepare "needle"; basically in attributes(trans)$dimnames[[2]] will be searched for the specie reaction term columns
#       needle <- paste("R", specie, "1", sep="")
#       # get start column
#       startc <- which(attributes(trans)$dimnames[[2]] == needle)
#       # get RX values for all layers for one timestep
#       for (j in seq(from = 0, to = parameters$N-1)){
#         rr_depth_p <- c(rr_depth_p, trans[i, startc+j])
#       }
#       
#     # get species transport rate depth profile; rates for different layers are in different columns; one row is one timestep
#       # prepare empty vector
#       dC_depth_p <- c()
#       # prepare "needle"; basically in attributes(trans)$dimnames[[2]] will be searched for the specie reaction term columns
#       needle <- paste("trandC_", specie, "1", sep="")
#       # get start column
#       startc <- which(attributes(trans)$dimnames[[2]] == needle)
#       # get RX values for all layers for one timestep
#       for (j in seq(from = 0, to = parameters$N-1)){
#         dC_depth_p <- c(dC_depth_p, trans[i, startc+j])
#       }
#     
#     # get "species-total-concentration-change-rate"
#       total_dC_depth_p <- rr_depth_p + dC_depth_p
#       
#     # depth integrate "species-total-concentration-change-rate": sum up depth profile and convert from [mol/(V_sf*y)] resp. [mol/(V_pw*y)] to [mol/(m^2*y)]
#       # set "volume-fraction conversion factor" (vfcf); converts 1/V_sf or 1/V_pw to 1/V_total
#       if (phase == "solute"){
#         vfcf <- grid_collection$por.grid$mid
#       } else {
#         vfcf <- grid_collection$svf.grid$mid
#       }
#       
#       # depth integrate for this timestep:
#       # d_int_dC: depth integrated change in concentration through chemical reactions (RX) + through transport processes (trandC) for one timestep
#       d_int_dC <- sum(total_dC_depth_p * vfcf * grid_collection$grid$dx) # [mol/(m²*yr)]
#     
#       # store d_int_dC in d_int_dCs
#       d_int_dCs <- c(d_int_dCs, d_int_dC)
#   }
#   
#   # a2) integration over time
#     # linear interpolation and integration
#     int_dC_func <- approxfun(parameters$times, d_int_dCs, method = "linear")
#     int_dC <- integrate(int_dC_func, lower = parameters$times[a], upper = parameters$times[b])
#   
#   # b) integrate fluxes at upper and lower boudnary from a to b
#   # linear interpolation and integration of model derived values is used for fluxes at lower boundary and solute-fluxes at upper boundary (they vary with time)
#   # constant interpolation and integration of model derived values is used for constant solid-fluxes at upper boundary (defined as constant)
#   # varying solid-fluxes are calculated by integration of the origiginal definition function (smaller timeintervalls than t_int can be defined for fluxes)
#   
#   # get model derived fluxes
#   # flux upwards
#   flux_up <- trans[, paste("FU_", specie, sep="")]
#   # flux downwards
#   flux_down <- trans[, paste("FD_", specie, sep="")]  
#   
#   # fluxes at upper sediment boundary
#   if (phase == "solute"){ # linear integration of model derived fluxes for solutes
#     flux_up_func <- approxfun(parameters$times, flux_up, method = "linear")
#     int_flx_up <- integrate(flux_up_func, lower = parameters$times[a], upper = parameters$times[b])
#   }
#   else { # solids
#     # check if a boundary condition function is defined (lookup in boundary_conditions$varying)
#     if (paste("F_", specie, sep="") %in% names(boundary_conditions$varying)){
#       # integrate defined function
#       int_flx_up <- integrate(f = Vectorize(boundary_conditions$varying[[paste("F_", specie, sep="")]]), lower = parameters$times[a], upper = parameters$times[b])
#     }
#     else {
#       # constant integration of model derived fluxes
#       flux_up_func <- approxfun(parameters$times, flux_up, method = "constant", f=0)
#       int_flx_up <- integrate(flux_up_func, lower = parameters$times[a], upper = parameters$times[b])
#     }
#   }
#   
#   # fluxes at lower sediment boundary: linear integration for all species
#   flux_down_func <- approxfun(parameters$times, flux_down, method = "linear")
#   int_flx_down <- integrate(flux_down_func, lower = parameters$times[a], upper = parameters$times[b])
# 
#   
#   # results
#   mb <- int_dC$value + int_flx_up$value - int_flx_down$value # mass balance
#   return(list(int_dC=int_dC, int_flx_up=int_flx_up, int_flx_down=int_flx_down, mb=mb))
# }
# 
# 
# ### create transient mass balance data frame
# mass_balance_transient_all <- function(){
#   trans_mass_balances <- data.frame(
#     specie = c(),
#     mass_balance = c()
#   )
#   for (specie in species_operational){
#     new_data <- data.frame(
#       specie = specie$name,
#       mass_balance = mass_balance_transient(specie = specie$name)$mb
#     )
#     trans_mass_balances <- rbind(trans_mass_balances, new_data)
#   }
#   trans_mass_balances <<- trans_mass_balances
# }


### molar based mass balance for elements
molar_mass_balance <- function(species_of_interest, mol_content, a, b){
  # species_of interest: all species that contain the element of interest; e.g. Fe: c("FeOH3A", "FeS", "FeS2", "FeP", "FeCO3", "VivP", "Fe_2")
  # mol_content: one mole FeS2 contains one mole Fe, whereas one mole vivianite contains three moles Fe; e.g. c(1, 1, 1, 1, 1, 3, 1)
  # a: startpoint of mass balance (has to be a timestep in parameters$times)
  # b: endpoint of mass balance (has to be a timestep in parameters$times)
  
  # get position of a and b in parameters$times
  a <- which(round(parameters$times, digits = 15) == round(a, digits = 15))
  b <- which(round(parameters$times, digits = 15) == round(b, digits = 15))
  
  # prepare mole and fluxes pools
  pool_a <- 0        # content of element in sediment at timestep  a
  pool_b <- 0        # content of element in sediment at timestep  b
  int_flxs_up <- 0   # summarized and time-integrated fluxes at upper sediment boundary
  int_flxs_down <- 0 # summarized and time-integrated fluxes at lower sediment boundary
  
  
  # for each specie
    # a) calculate content of element in sediment at timestep a and b
    # b) integrate fluxes at upper and lower boudnary from a to b
    # c1) c2) add results to pools
  
  for (specie in species_of_interest){
    # a) # get depth integrated mol number for the first and the last timestep
      # get columns of interest (one row is one timestep)
      columns <- which(attributes(trans)$dimnames[[2]] == specie)
      
      # check for species phase
      if (species_operational[[specie]]$phase == "solute"){
        phase <- "solute"
      } else {
        phase <- "solid"
      }
      
      # set "volume-fraction conversion factor" (vfcf); converts 1/V_sf or 1/V_pw to 1/V_total
      if (phase == "solute"){
        vfcf <- grid_collection$por.grid$mid
      } else {
        vfcf <- grid_collection$svf.grid$mid
      }
      
      # depth integrate molar content for timestep a:
      d_int_mol_a <- sum(trans[a, columns] * vfcf * grid_collection$grid$dx) # [mol/m²]
      # depth integrate molar content for timestep a:
      d_int_mol_b <- sum(trans[b, columns] * vfcf * grid_collection$grid$dx) # [mol/m²]
      
      # c1) add depth integrated molar content to pools
        # get position of specie in "species_of_interest"-vector to know mol content
        position <- which(species_of_interest == specie)
        # add to pools
        pool_a <- pool_a + d_int_mol_a * mol_content[position]
        pool_b <- pool_b + d_int_mol_b * mol_content[position]
        
    # b) integrate fluxes at upper and lower boundary from a to b
      # linear interpolation and integration of model derived values is used for fluxes at lower boundary and solute-fluxes at upper boundary (they vary with time)
      # constant interpolation and integration of model derived values is used for constant solid-fluxes at upper boundary (defined as constant)
      # varying solid-fluxes are calculated by integration of the origiginal definition function (smaller timeintervalls than t_int can be defined for fluxes)
      
      # get model derived fluxes
        # flux upwards
        flux_up <- trans[, paste("FU_", specie, sep="")]
        # flux downwards
        flux_down <- trans[, paste("FD_", specie, sep="")]  
      
      # fluxes at upper sediment boundary
      if (phase == "solute"){ # linear integration of model derived fluxes for solutes
        flux_up_func <- approxfun(parameters$times, flux_up, method = "linear")
        int_flx_up <- integrate(flux_up_func, lower = parameters$times[a], upper = parameters$times[b])
      }
      else { # solids
        # check if a boundary condition function is defined (lookup in boundary_conditions$varying)
        if (paste("F_", specie, sep="") %in% names(boundary_conditions$varying)){
          # integrate defined function
          int_flx_up <- integrate(f = Vectorize(boundary_conditions$varying[[paste("F_", specie, sep="")]]), lower = parameters$times[a], upper = parameters$times[b])
        }
        else {
          # constant integration of model derived fluxes
          flux_up_func <- approxfun(parameters$times, flux_up, method = "constant", f=0)
          int_flx_up <- integrate(flux_up_func, lower = parameters$times[a], upper = parameters$times[b])
        }
      }
        
      # fluxes at lower sediment boundary: linear integration for all species
        flux_down_func <- approxfun(parameters$times, flux_down, method = "linear")
        int_flx_down <- integrate(flux_down_func, lower = parameters$times[a], upper = parameters$times[b])
        
      # c2) add time integrated fluxes to pools
        int_flxs_up <- int_flxs_up + int_flx_up$value * mol_content[position]
        int_flxs_down <- int_flxs_down + int_flx_down$value * mol_content[position]
  }
  
  # results
  mol_flux_up <- pool_b - pool_a + int_flxs_down # flux at upper boundary calculated by molar mass balance
  mb <- pool_a - pool_b + int_flxs_up - int_flxs_down # mass balance
  return(list(pool_a=pool_a, pool_b=pool_b, int_flxs_up=int_flxs_up, int_flxs_down=int_flxs_down, mol_flux_up=mol_flux_up, mb=mb))
}



#*************************************
# calculate mass balances
#*************************************

# # steady-state mass balance: single call
# mass_balance_steady("FeS2")

# steady-state mass balance: all species
mass_balance_steady_all()

# # # transient mass balance: single call
#  mass_balance_transient("FeOH3A")
# 
# # transient mass balance: all species
# mass_balance_transient_all()

# molar mass balance for iron for the first two years (without vivianite)
molar_mass_balance(c("FeOH3A", "FeS", "FeS2", "FeCO3", "Fe_2"), c(1, 1, 1, 1, 1), 0, 2)

# # molar mass balance for oxygen for the first two years
# molar_mass_balance(c("OrgCA", "FeOH3A", "FeP", "FeCO3", "VivP", "MnO2A", "MnCO3", "O2", "SO4", "NO3", "PO4", "DIC"), c(-1.694778, 3, 7, 3, 8, 2, 3, 2, 4, 3, 4, 3), 0, 2)
