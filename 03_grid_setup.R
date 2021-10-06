
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
  source(file=configs$parameters_config, local=TRUE)
  
  # create "grid_collection"-list: stores grid and grid properties
  grid_collection <- list()
  
  # setup grid
  #grid <- setup.grid.1D(x.up = 0, L = L, N = N)
  grid <- setup.grid.1D(x.up = 0, L = 0.3, N = 30, p.dx.1 = 20, dx.1 = 0.0002) # temporary higher resolution at SWI to fit O2 data
  
  # attach bioturbation-coefficient
  #Db.grid <- setup.prop.1D(value = Db, grid = grid)
  Db.grid <- setup.prop.1D(func=p.sig, grid = grid, y.0 = Db, y.inf = 0,x.L = 0.175, x.att = 0.05) # Db=0, no bioturbation
  
  # attach porosity
  por.grid <- setup.prop.1D(func=p.exp, grid=grid, y.0 = por.0, y.inf = por.inf, x.att = por_shape)
  
  # attach solid volume fraction
  svf.grid <-setup.prop.1D(func=p.exp, grid=grid, y.0 = 1-por.0, y.inf = 1-por.inf, x.att = por_shape)
  
  # attach sediment density
  sed_dens.grid <- setup.prop.1D(func=p.exp, grid=grid, y.0 = 2.25041639, y.inf = 2.33802349, x.att = 0.09882358)
  
  # attach diffusive parameters for advection at top
  #now deliver the same values as porosity does not change with depth; to check correct OM degradation sequences
  #if porosity changes with depth then it will vary
  dummy <- setup.compaction.1D(v.0 = v, por.0=por.0, por.inf=por.inf, por.grid=por.grid)
  v.grid <- dummy$v   # solid phase advective velocities
  u.grid <- dummy$u   # dissolved phase advective velocities
  
  
  # prepare temperature-dependent diffusion coefficients for solute species:
    # a) dummy variables in "grid_collection"-list (DX.grid),
    # b) function to calculate values in model-function (solute_diffusion_coffs(t)), because Dmol.X is temperature-dependent (defined outside this function)
  # approach:
    # diffusion coefficient for solutes: DX = Dmol.X + Db, where Dmol.X is the molecular diffusion coefficient calculated by diffcoeff-function of the marelac package ...
    # ... which is also corrected for tortuosity and adjusted for our time unit
      # Dmol.X <- diffcoeff(S = S, t = TC, P = P, species = name_diffcoeff)[[name_diffcoeff]] * sectoyr/tort 
      # tort <- 1 - 2*log(por.0)  correction for tortuosity
      # sectoyr <- 3600*24*365.25  conversion from s to yr; diffcoeff returns ionic diffusion coefficients in m2/s
  
  # a) set dummy-grid-properties for solute species
  for (i in seq_along(occurring_species)){
    if (occurring_species[[i]]$phase == "solute"){
      # construct "Dx.grid" variable name, e.g. DO2.grid
      name <- occurring_species[[i]]$abbreviation # species name
      name <- paste("D", name, ".grid", sep = "") # variable name
      # create dummy-grid-property DX.grid ...
      DX.grid <- setup.prop.1D(value = "dummy",  grid = grid)
      # ... and attach it to "grid_collection"-list
      grid_collection[[name]] <- DX.grid
    }
  }
  
  
  # put all properties and corr in list and send it to .GlobalEnv
  grid_collection <<- c(grid_collection, list(grid=grid, Db.grid=Db.grid, por.grid=por.grid, svf.grid=svf.grid, v.grid=v.grid, u.grid=u.grid, sed_dens.grid=sed_dens.grid))
}

grid_setup()

# b) define function that will be used in model-function to calculate temperature dependent diffusion coefficients for solutes
grid_collection$solute_diffusion_coffs <- function(TC){
  for (i in seq_along(occurring_species)){
    if (occurring_species[[i]]$phase == "solute"){
      # get species name for diffcoff-function
      name_diffcoeff <- occurring_species[[i]]$abbr_diffcoeff
      # calculate molecular diffusion coefficient
      Dmol.X <- diffcoeff(S = parameters$S, t = TC, P = parameters$P, species = name_diffcoeff)[[name_diffcoeff]]
      # correct molecular diffusion coefficient...
      corr <- function(por){Dmol.X*3600*24*365.25/(1 - 2*log(por))}
      DX <- lapply(grid_collection$por.grid, FUN = corr)
      # ...and add bioturbation
      DX <- mapply("+", DX, grid_collection$Db.grid)
      # replace dummy vectors by calculated ones
      # get species varaible name in list
      name <- occurring_species[[i]]$abbreviation # species name
      name <- paste("D", name, ".grid", sep = "") # variable name
      # replace DX.grid$mid and DX.grid$int
      grid_collection[[name]]$mid <<- DX$mid
      grid_collection[[name]]$int <<- DX$int
    }
  }
}


#***************************
# clean_up
#***************************
# clear workspace: remove function
rm(grid_setup)