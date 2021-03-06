
###########################################################################################################
#                                           RUN MODEL FROM HERE                                           #
###########################################################################################################


### TODO: set working directory
  setwd("/home/robert/Dokumente/SedimentModel")
  wd_path <- getwd()

### TODO: set links to config files
  configs <- list(
    parameters_config = "011_parameters_config.R",
    chemical_base_config = "021_chemical_base_config.R",
    boundary_conditions_config = "051_boundary_conditions_config.R" 
  )
  
### set up model
  ## set up the chemical base: occurring species and reactions, create a diagram and have a look at it
    source("012_parameters_func.R")
    source("022_chemical_base_func.R")
  
  # select reactions by name:
  chemical_base_main(specify=TRUE, list("E1", "E2", "E3", "E4a", "E4b", "E5", "E6"))
  
  # diagram
    source("023_chemical_base_draw_diagram.R")
      # export to html-file: requires "pandoc"
      visSave(model_diagram, file=paste(wd_path, "/exports/interactive_diagram/model.html", sep=""), selfcontained=TRUE, background="white")
      # view in RStudio
      model_diagram
     
  ## attach parameters to grid
    source("03_grid_setup.R")
  ## create processing lists
    source("04_list_preparation.R")
  
### source model-function
      source("06_model_function.R")
      
### checks before model run
  ## check porosity profile
      plot(grid_collection$por.grid, grid = grid_collection$grid, xyswap = TRUE)
      
### Solve the model: Steady state
print(system.time(
  ss <- steady.1D(y = state,
                  time = 0.2, # oxic steady state
                  func = Model, 
                  parms = NULL, 
                  names = names_out, 
                  method = "stode", # stodes, when P adsorption is activated
                  #verbose=TRUE,
                  pos = TRUE,
                  nspec = length(names_out) #,rtol = 1e-16,ctol = 1e-16 ,atol = 1e-16
  )))
state <- as.vector(ss$y) # save time on iterative runs with same species

### extract steady state solution as input for transient model
source('07_prepare_transient_input.R')

### Solve transient model by steady state beginning
print(system.time(
  trans <- ode.1D(y = da_ss, 
                  time = parameters$times, 
                  func = Model, 
                  parms = NULL, 
                  names = names_out,
                  #method = "lsoda", 
                  #verbose = TRUE, 
                  nspec = length(names_out),
                  dimens = parameters$N
                  #,rtol = 1e-7, atol = 1e-6
  )))

### add names to concentration columns
source('08_trans_data_processing.R')
