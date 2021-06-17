### setting wd
  setwd("/home/robert/Dokumente/SedimentModel")
  wd_path <- getwd()

### load model-setup
  source("01_model_setup.R")

### create diagram
  source("02_draw_diagram.R")
  
  # export to html-file: requires "pandoc"
  visSave(model_diagram, file=paste(wd_path, "/exports/interactive_diagram/model.html", sep=""), selfcontained=TRUE, background="white")
  
  # view in RStudio
  model_diagram
  