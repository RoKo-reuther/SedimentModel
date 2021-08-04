
###########################################################################################################
#                           DONT CHANGE ANYTHING HERE WITHOUT INTENDED PURPOSE                            #
###########################################################################################################

library(shape)
library(rootSolve)
library(deSolve)
library(marelac)
library(ReacTran)

# create grid and attach parameters to grid
grid_setup <- function(){
  # load environmental and grid parameters to function environment
  source(file="01_parameters_config.R", local=TRUE)
  
  # create "grid_collection"-list: stores grid and grid properties
  grid_collection <- list()
  
  # setup grid
  grid <- setup.grid.1D(x.up = 0, L = L, N = N)
  
  # attach bioturbation-coefficient
  Db.grid <- setup.prop.1D(value = Db, grid = grid)
  # Db.grid <- setup.prop.1D(func=p.sig, grid = grid, y.0 = Db, y.inf = 0,x.L = 10, x.att = 4) # Db=0, no bioturbation
  
  # attach porosity
  por.grid <- setup.prop.1D(value = por, grid = grid)
  # por.grid <- setup.prop.1D(func=Porosity, grid=grid, y.0 = por.0, y.inf = por.inf, gamma=gamma)
  # por.grid <- setup.prop.1D(func=p.exp, grid=grid, y.0 = por.0, y.inf = por.inf)
  
  # attach solid volume fraction
  svf.grid <- setup.prop.1D(value = svf, grid = grid)
  # svf.grid <- setup.prop.1D(func=SolidVolumeFrac, grid=grid, y.0 = por.0, y.inf = por.inf, gamma=gamma)
  # a temporary workaround .. first: svf.grid = por.grid; second: calculate new values for svf.grid content "1-por"
  # svf.grid <- setup.prop.1D(func=p.exp, grid=grid, y.0 = por.0, y.inf = por.inf)
  # for (i in seq_along(svf.grid$mid)){
  #   svf.grid$mid[i] <- 1-svf.grid$mid[i]
  # }
  # for (i in seq_along(svf.grid$int)){
  #   svf.grid$int[i] <- 1-svf.grid$int[i]
  # }
  
  # attach diffusive parameters for advection at top
  #now deliver the same values as porosity does not change with depth; to check correct OM degradation sequences
  #if porosity changes with depth then it will vary
  dummy <- setup.compaction.1D(v.0 = v, por.0=por, por.inf=por.inf, por.grid=por.grid)
  v.grid <- dummy$v                               # solid phase advective velocities
  u.grid <- dummy$u                               # dissolved phase advective velocities
  
  # calculate diffusion coefficient and attach depth diffusion coefficient for solute species
  sectoyr   <- 3600*24*365.25 # conversion from s to yr
  tort      <- 1 - 2*log(por) # tortuosity (used to correct Diff.coeff.)
  # go through occuring species by index
  for (i in seq_along(occuring_species)){
    if (occuring_species[[i]]$phase == "solute"){
      name <- occuring_species[[i]]$abbreviation # species name
      name_diffcoeff <- occuring_species[[i]]$abbr_diffcoeff # species name
      # calculate diffusion coefficients [m2/yr]; diffcoeff returns ionic diffusion coefficients in m2/s
      Dmol.X <- diffcoeff(S = S, t = TC, P = P, species = name_diffcoeff)[[name_diffcoeff]] * sectoyr/tort
      # and attach DX.grid to "grid_collection"-list
      DX.grid <- setup.prop.1D(value = Dmol.X + Db,  grid = grid)
      name <- paste("D", name, ".grid", sep = "") # list name, e.g. DO2.grid
      grid_collection[[name]] <- DX.grid
    }
  }
  
  # put all properties in list and send it to .GlobalEnv
  grid_collection <<- c(grid_collection, list(grid=grid, Db.grid=Db.grid, por.grid=por.grid, svf.grid=svf.grid, v.grid=v.grid, u.grid=u.grid))
}

grid_setup()

#***************************
# clean_up
#***************************
# clear workspace: remove function
rm(grid_setup)