
###########################################################################################################
#                           NAME TRANSIENT DATA FRAME COLUMNS (SPECIES COLUMNS)                           #
###########################################################################################################

name_trans_columns <- function(){
  
  # copy current names-list
  new_names <- attributes(trans)$dimnames[[2]]
  
  # edit list: replace numbers through X_N
  counter <- 1
  for (specie in species_operational){
    new_names[((counter-1)*N+1):(N*counter)+1] <- rep(specie$name, N)
    counter <- counter +1
  }
  
  # replace dimnames-attribute
  attributes(trans)$dimnames[[2]] <<- new_names
}

name_trans_columns()

rm(name_trans_columns)