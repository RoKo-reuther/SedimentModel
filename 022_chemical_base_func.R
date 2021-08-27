
###########################################################################################################
#                           DONT CHANGE ANYTHING HERE WITHOUT INTENDED PURPOSE                            #
###########################################################################################################


#*************************************
# main func
#*************************************
chemical_base_main <- function(specify=FALSE, reaction_list){
  # load parameters to function environment
  source(file=configs$parameters_config, local=TRUE)
  source(file=configs$chemical_base_config, local=TRUE)
  
  # use this arguments to create model with only explicitly named reactions
  # e.g. chemical_base_main(specify=TRUE, list("E1", "E9b"))
  if (specify ==TRUE) {
    for (reaction in reactions_collection){
      if (any(reaction_list==reaction$abbreviation)){
        # do nothing
      }
      else {
        reactions_collection[[reaction$abbreviation]]$activated <- FALSE
      }
    }
  }

  #*************************************
  # create "occuring_reactions"-list
  #*************************************
  # Create list with occuring reactions based on activated reactions and species in "chemical_base_config"-script.
  # Reactions can't occur if at least one educt is missing (required species defined in reactions_collection).
  # This can happen, if a species was disabled manually in species_collection (activated=FALSE).
  
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


  #*************************************
  # create "occuring_species"-list
  #*************************************
  # Create list with occuring species based on occuring-reactions list.
  
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


  #*************************************
  # adjust created lists
  #*************************************
  # adjust "occuring_species$involved_in"-list and "occuring_reactions$involved_species"-list
  # 1) add occuring reactions to "occuring_species$involved_in"-list
  # 2) delete not occuring species out of "occuring_reactions$involved_species"-list

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
        occuring_reactions[[reaction$abbreviation]]$involved_species$products[species$abbreviation] <<- NULL
      }
    }
  }
}

#chemical_base_main(specify=TRUE, list("E1", "E2", "E3", "E4a", "E4b", "E5", "E6", "E8", "E9a", "E9b", "E12", "E14a1", "E14a2", "E15", "E19", "E27", "E28", "E29a1", "E29a2", "E30a", "E32a", "E22", "E33", "E24", "E25", "E26", "E10", "E11", "E16", "E18", "E23"))
chemical_base_main()

#***************************
# clean_up
#***************************
# clear workspace: remove auxiliary variables and collections
rm(chemical_base_main)
