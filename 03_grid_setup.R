
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
  grid <- setup.grid.1D(x.up = 0, L = L, N = N)
  
  # attach bioturbation-coefficient
  Db.grid <- setup.prop.1D(value = Db, grid = grid)
  # Db.grid <- setup.prop.1D(func=p.sig, grid = grid, y.0 = Db, y.inf = 0,x.L = 10, x.att = 4) # Db=0, no bioturbation
  
  # attach porosity
  por.grid <- setup.prop.1D(func=p.exp, grid=grid, y.0 = por.0, y.inf = por.inf, x.att = por_shape)
  
  # attach solid volume fraction
  svf.grid <-setup.prop.1D(func=p.exp, grid=grid, y.0 = 1-por.0, y.inf = 1-por.inf, x.att = por_shape)
  
  # attach diffusive parameters for advection at top
  #now deliver the same values as porosity does not change with depth; to check correct OM degradation sequences
  #if porosity changes with depth then it will vary
  dummy <- setup.compaction.1D(v.0 = v, por.0=por.0, por.inf=por.inf, por.grid=por.grid)
  v.grid <- dummy$v   # solid phase advective velocities
  u.grid <- dummy$u   # dissolved phase advective velocities
  
  
  # prepare temperature-dependent diffusion coefficients for solute species:
    # a) dummy variables in "grid_collection"-list (DX.grid),
    # b) porosity dependent correction factor (corr) and
    # c) function to calculate values in model-function (solute_diffusion_coffs(t)), because Dmol.X is temperature-dependent (defined outside this function)
  # approach:
    # diffusion coefficient for solutes: DX = Dmol.X + Db, where Dmol.X is the molecular diffusion coefficient calculated by diffcoeff-function of the marelac package ...
    # ... which is also corrected for tortuosity and adjusted for our time unit
      # Dmol.X <- diffcoeff(S = S, t = TC, P = P, species = name_diffcoeff)[[name_diffcoeff]] * sectoyr/tort 
      # tort <- 1 - 2*log(por.0)  correction for tortuosity
      # sectoyr <- 3600*24*365.25  conversion from s to yr; diffcoeff returns ionic diffusion coefficients in m2/s
  
  # a) set dummy-grid-properties for solute species
  for (i in seq_along(occuring_species)){
    if (occuring_species[[i]]$phase == "solute"){
      # construct "Dx.grid" variable name, e.g. DO2.grid
      name <- occuring_species[[i]]$abbreviation # species name
      name <- paste("D", name, ".grid", sep = "") # variable name
      # create dummy-grid-property DX.grid ...
      DX.grid <- setup.prop.1D(value = "dummy",  grid = grid)
      # ... and attach it to "grid_collection"-list
      grid_collection[[name]] <- DX.grid
    }
  }
  
  # b) calculate correction factor for varying porosity: corr = sectoyr/tort
  corr <- lapply(por.grid, FUN = function(por){3600*24*365.25/(1 - 2*log(por))})
  
  # put all properties and corr in list and send it to .GlobalEnv
  grid_collection <<- c(grid_collection, list(grid=grid, Db.grid=Db.grid, por.grid=por.grid, svf.grid=svf.grid, v.grid=v.grid, u.grid=u.grid, diff_calculations=list(corr=corr)))
}

grid_setup()

# c) define function that will be used in model-function to calculate temperature dependent diffusion coefficients for solutes
grid_collection$diff_calculations$solute_diffusion_coffs <- function(){
  for (i in seq_along(occuring_species)){
    if (occuring_species[[i]]$phase == "solute"){
      # get species name for diffcoff-function
      name_diffcoeff <- occuring_species[[i]]$abbr_diffcoeff
      # calculate molecular diffusion coefficient
      Dmol.X <- diffcoeff(S = S, t = TC, P = P, species = name_diffcoeff)[[name_diffcoeff]]
      # correct molecular diffusion coefficient and add bioturbation
      DX <- lapply(grid_collection$diff_calculations$corr, FUN = function(corr){corr*Dmol.X+Db})
      # replace dummy vectors by calculated ones
      # get species varaible name in list
      name <- occuring_species[[i]]$abbreviation # species name
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