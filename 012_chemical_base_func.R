
###########################################################################################################
#                           DONT CHANGE ANYTHING HERE WITHOUT INTENDED PURPOSE                            #
###########################################################################################################


#*************************************
# select reactions by name
#*************************************
# use this function to create model with only explicitly named reactions in a list
# how to use:
# 1) load create_orl(), create_osl(), adjust_lists() and this function
# 2) then run e.g. set_reactions_by_name(list("E1", "E2"))
set_reactions_by_name <- function(reaction_list){
  source("011_chemical_base_config.R")
  for (reaction in reactions_collection){
    if (any(reaction_list==reaction$abbreviation)){
      # do nothing
    }
    else {
      reactions_collection[[reaction$abbreviation]]$activated <<- FALSE
    }
  }
  create_orl()
  create_osl()
  adjust_lists()
}


#*************************************
# create "occuring_reactions"-list
#*************************************
# Create list with occuring reactions based on activated reactions and species in "01_model_setup_config"-script.
# Reactions can't occur if at least one educt is missing (required species defined in reactions_collection).
# This can happen, if a species was disabled manually in species_collection (activated=FALSE).

create_orl <- function(){
  # create empty list
  occuring_reactions <<- list()
  
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
        if (species_collection[[species$abbreviation]]$activated==FALSE){
          # checkup is set FALSE if at least one educt is deactivated
          checkup <- FALSE
        }
      }
      # use checkup to decide if reaction can be added to occuring_reactions-list
      if (checkup==TRUE){
        # add reaction to occuring_reactions-list...
        occuring_reactions <<- c(occuring_reactions, list(reaction))
        # ...and set reaction name to make occuring_reactions-data clearer
        names(occuring_reactions)[length(occuring_reactions)] <<- reaction$abbreviation
      }
    } 
  }
}
create_orl()


#*************************************
# create "occuring_species"-list
#*************************************
# Create list with occuring species based on occuring-reactions list.

create_osl <- function(){
  # create empty list
  occuring_species <<- list()
  
  # go through occuring reactions...
  for (reaction in occuring_reactions){
    # add involved species to "occuring_species"-list, if it is not in yet and not manually deactivated
    for (species in c(reaction$involved_species$educts, reaction$involved_species$products)){
      if ((!exists(species$abbreviation, occuring_species))&(species_collection[[species$abbreviation]]$activated==TRUE)){
        occuring_species <<- c(occuring_species, species_collection[species$abbreviation])
      }
    }
  }
}
create_osl()


#*************************************
# adjust created lists
#*************************************
# adjust "occuring_species$involved_in"-list and "occuring_reactions$involved_species"-list
# 1) add occuring reactions to "occuring_species$involved_in"-list
# 2) delete not occuring species out of "occuring_reactions$involved_species"-list

adjust_lists <- function(){
  # go through occuring reactions...
  for (reaction in occuring_reactions){
    for (species in c(reaction$involved_species$educts, reaction$involved_species$products)){
      # check if species is in "occuring_species"-list
      if (exists(species$abbreviation, occuring_species)){
        # add occuring reactions to "occuring_species$involved_in"-list
        occuring_species[[species$abbreviation]]$involved_in <<- c(occuring_species[[species$abbreviation]]$involved_in, reaction$abbreviation)
      }
      else {
        # delete not occuring species out of "occuring_reactions$involved_species"-list (that only can be products ensured by selection of occuring reactions)
        occuring_reactions[[reaction$abbreviation]]$involved_species$products[species$abbreviation] <<- NULL ###!funzt gerade noch nicht, weil name erst nachher gesetzt wird
      }
    }
  }
}
adjust_lists()


#***************************
# clean_up
#***************************
# clear workspace: remove auxiliary variables and collections
rm(reactions_collection, species_collection, create_orl, create_osl, adjust_lists, set_reactions_by_name)
