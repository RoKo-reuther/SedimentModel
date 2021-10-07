
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

  #**************************************
  # name sublists of "collection"-lists
  #**************************************
  for (i in seq_along(species_collection)){
    names(species_collection)[i] <- species_collection[[i]]$abbreviation
  }
  for (i in seq_along(reactions_collection)){
    names(reactions_collection)[i] <- reactions_collection[[i]]$abbreviation
  }
  
  
  #*************************************
  # create "occurring_reactions"-list
  #*************************************
  # Create list with occurring reactions based on activated reactions and species in "chemical_base_config"-script.
  # Reactions can't occur if at least one educt is missing (required species defined in reactions_collection).
  # This can happen, if a species was disabled manually in species_collection (activated=FALSE).
  
  # create empty list
  occurring_reactions <<- list()
  
  # create "occurring_reactions"-list
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
      # use checkup to decide if reaction can be added to occurring_reactions-list
      if (checkup==TRUE){
        # add reaction to occurring_reactions-list...
        occurring_reactions <<- c(occurring_reactions, list(reaction))
        # ...and set reaction name to make occurring_reactions-data clearer
        names(occurring_reactions)[length(occurring_reactions)] <<- reaction$abbreviation
      }
    } 
  }


  #*************************************
  # create "occurring_species"-list
  #*************************************
  # Create list with occurring species based on occurring-reactions list.
  
  # create empty list
  occurring_species <<- list()
  
  # go through occurring reactions...
  for (reaction in occurring_reactions){
    # add involved species to "occurring_species"-list, if it is not in yet and not manually deactivated
    for (species in c(reaction$involved_species$educts, reaction$involved_species$products)){
      if ((!exists(species$abbreviation, occurring_species))&(species_collection[[species$abbreviation]]$activated==TRUE)){
        occurring_species <<- c(occurring_species, species_collection[species$abbreviation])
      }
    }
  }


  #*************************************
  # adjust created lists
  #*************************************
  # adjust "occurring_species$involved_in"-list and "occurring_reactions$involved_species"-list
  # 1) add occurring reactions to "occurring_species$involved_in"-list
  # 2) delete not occurring species out of "occurring_reactions$involved_species"-list

  # go through occurring reactions...
  for (reaction in occurring_reactions){
    for (species in c(reaction$involved_species$educts, reaction$involved_species$products)){
      # check if species is in "occurring_species"-list
      if (exists(species$abbreviation, occurring_species)){
        # add occurring reactions to "occurring_species$involved_in"-list
        occurring_species[[species$abbreviation]]$involved_in <<- c(occurring_species[[species$abbreviation]]$involved_in, reaction$abbreviation)
      }
      else {
        # delete not occurring species out of "occurring_reactions$involved_species"-list (that only can be products ensured by selection of occurring reactions)
        occurring_reactions[[reaction$abbreviation]]$involved_species$products[species$abbreviation] <<- NULL
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
