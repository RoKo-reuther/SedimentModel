
###########################################################################################################
#                           DONT CHANGE ANYTHING HERE WITHOUT INTENDED PURPOSE                            #
###########################################################################################################


#*************************************
# select reactions by name
#*************************************
# use this function to create model with only explicitly named reactions in a list
# how to use: reactions_collection <- set_reactions_by_name(list("E1", "E2"))
# then run the rest of this script
set_reactions_by_name <- function(reaction_list){
  source("01_model_setup_config.R")
  for (reaction in reactions_collection){
    if (any(reaction_list==reaction$abbreviation)){
      # do nothing
    }
    else {
      reactions_collection[[reaction$abbreviation]]$activated <- FALSE
    }
  }
  return(reactions_collection)
}


#*************************************
# create "occuring_reactions"-list
#*************************************
# Create list with occuring reactions based on activated reactions and species in "01_model_setup_config"-script.
# Reactions can't occur if at least one educt is missing (required species defined in reactions_collection).
# This can happen, if a species was disabled manually in species_collection (activated=FALSE).

# create empty list
occuring_reactions <- list()

# create "occuring_reactions"-list
# go through reactions...
for (reaction in reactions_collection){
  # ...and check if reaction is activated
  if (reaction$activated==TRUE){
    # have a look at the required species (educts)
    # and check whether the species required for the reaction are activated
    # auxiliary variable checkup: are ALL required species activated?"
    checkup <- TRUE
    for (species in reaction$involved_species$educts){
      if (species_collection[[species]]$activated==FALSE){
        # checkup is set FALSE if at least one educt is deactivated
        checkup <- FALSE
      }
    }
    # use checkup to decide if reaction can be added to occuring_reactions-list
    if (checkup==TRUE){
      # add reaction to occuring_reactions-list...
      occuring_reactions <- c(occuring_reactions, list(reaction))
      # ...and set reaction name to make occuring_reactions-data clearer
      names(occuring_reactions)[length(occuring_reactions)] <- reaction$abbreviation
    }
  } 
}


#*************************************
# create "occuring_species"-list
#*************************************
# Create list with occuring species based on occuring-reactions list.

# create empty list
occuring_species <- list()

# go through occuring reactions...
for (reaction in occuring_reactions){
  # add involved species to "occuring_species"-list, if it is not in yet and not manually deactivated
  for (species in c(reaction$involved_species$educts, reaction$involved_species$products)){
    if ((!exists(species, occuring_species))&(species_collection[[species]]$activated==TRUE)){
      occuring_species <- c(occuring_species, species_collection[species])
    }
  }
}


#*************************************
# adjust created lists
#*************************************
# adjust "occuring_species$involved_in"-list and "occuring_reactions$involved_species"-list
# 1) add occuring reactions to "occuring_species$involved_in"-list
# 2) delete not occuring species out of "occuring_reactions$involved_species"-list
# 3) set species names in in "occuring_reactions$involved_species"-list

# go through occuring reactions...
for (reaction in occuring_reactions){
  for (species in c(reaction$involved_species$educts, reaction$involved_species$products)){
    # check if species is in "occuring_species"-list
    if (exists(species, occuring_species)){
      # add occuring reactions to "occuring_species$involved_in"-list
      occuring_species[[species]]$involved_in <- c(occuring_species[[species]]$involved_in, reaction$abbreviation)
    }
    else {
      # delete not occuring species out of "occuring_reactions$involved_species"-list (that only can be products ensured by selection of occuring reactions)
      occuring_reactions[[reaction$abbreviation]]$involved_species$products[species] <- NULL
    }
  }
  # set species names in in "occuring_reactions$involved_species"-list to make this list clearer and enable to use "exists"-function later on
  # take account for species type: go through "types" and per type through species by number
  typen <- c("educts", "products")
  for (type in typen){
    # count educts / products and store number
    number <- length(occuring_reactions[[reaction$abbreviation]]$involved_species[[type]])
    for (i in seq_len(number)){
      names(occuring_reactions[[reaction$abbreviation]]$involved_species[[type]])[i] <- occuring_reactions[[reaction$abbreviation]]$involved_species[[type]][[i]]
    }
  }
}


#***************************
# clean_up
#***************************
# clear workspace: remove auxiliary variables and collections
  rm(reactions_collection, species_collection, reaction, species, checkup, i, number, type, typen, set_reactions_by_name)
