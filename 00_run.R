
###########################################################################################################
#                                           RUN MODEL FROM HERE                                           #
###########################################################################################################


### TODO: set working directory
  setwd("/home/robert/Dokumente/SedimentModel")
  wd_path <- getwd()

### set up model
  ## set up the chemical base: occuring species and reactions, create a diagram and have a look at it
    source("011_chemical_bas_config.R")
    source("012_chemical_base_func.R")
    source("013_chemical_base_draw_diagram.R")
      # export to html-file: requires "pandoc"
      visSave(model_diagram, file=paste(wd_path, "/exports/interactive_diagram/model.html", sep=""), selfcontained=TRUE, background="white")
      # view in RStudio
      model_diagram
    
  ## set up model grid
  