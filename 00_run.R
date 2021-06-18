
###########################################################################################################
#                                           RUN MODEL FROM HERE                                           #
###########################################################################################################


### TODO: set working directory
  setwd("/home/robert/Dokumente/SedimentModel")
  wd_path <- getwd()

### set up model
  source("01_model_setup_config.R")
  source("01_model_setup_func.R")
  

### create diagram
  source("02_draw_diagram.R")
  
  # export to html-file: requires "pandoc"
  visSave(model_diagram, file=paste(wd_path, "/exports/interactive_diagram/model.html", sep=""), selfcontained=TRUE, background="white")
  
  # view in RStudio
  model_diagram
  