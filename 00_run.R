
###########################################################################################################
#                                           RUN MODEL FROM HERE                                           #
###########################################################################################################


### TODO: set working directory
  setwd("/home/robert/Dokumente/SedimentModel")
  wd_path <- getwd()

### set up model
  ## set up the chemical base: occuring species and reactions, create a diagram and have a look at it
    source("022_chemical_base_func.R")
    source("023_chemical_base_draw_diagram.R")
      # export to html-file: requires "pandoc"
      visSave(model_diagram, file=paste(wd_path, "/exports/interactive_diagram/model.html", sep=""), selfcontained=TRUE, background="white")
      # view in RStudio
      model_diagram
  ## attach parameters to grid
    source("03_parameters2grid_func.R")
  ## create processing lists
    source("041_model_preparation.R")
      
### source transient-scenario-function (used to calculate boundary conditions also in steady-state)
      source("o_Model_transient_scenario.R")
      
### source boundary conditions
      source("o_Model_boundary.R")
  
### source model-function
      source("042_model_function.R")
      
### Solve the model: Steady state
print(system.time(
  ss <- steady.1D(y = state, 
                  func = Model, 
                  parms = NULL, 
                  names = names_out, 
                  #method = "stode",
                  pos = TRUE,
                  nspec = length(names_out) #,rtol = 1e-16,ctol = 1e-16 ,atol = 1e-16
  )
))


#load transient parts
source('o_Model_parameters_transient.R')
source('o_Model_initialize_transient.R')

#Solve transient model 
#by steady state beginning
print(system.time(
  trans <- ode.1D(y = da_ss, 
                  time = times, 
                  func = Model, 
                  parms = NULL, 
                  names = names_out,
                  #method = "lsoda", 
                  #verbose = TRUE, 
                  nspec = length(names_out)
                  #,rtol = 1e-7, atol = 1e-6
  )))
