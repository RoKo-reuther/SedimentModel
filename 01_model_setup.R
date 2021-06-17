
# define species / pools
  # create an entry in the following form for every species considered
  # "dummy_species"=list(abbreviation="dummy_species", name="dummy species", involved_in=list(), activated=TRUE)
species_collection <- list(
  "OM"=list(abbreviation="OM", name="organic matter", involved_in=list(), activated=TRUE),
  "O2"=list(abbreviation="O2", name="oxygen", involved_in=list(), activated=TRUE),
  "CO2"=list(abbreviation="CO2", name="carbon dioxide", involved_in=list(), activated=TRUE),
  "NH4+"=list(abbreviation="NH4+", name="ammonium", involved_in=list(), activated=TRUE),
  "PO4"=list(abbreviation="PO4", name="phosphate (pool)", involved_in=list(), activated=TRUE),
  "NO3-"=list(abbreviation="NO3-", name="nitrate", involved_in=list(), activated=TRUE),
  "N2"=list(abbreviation="N2", name="nitrogen", involved_in=list(), activated=TRUE),
  "MnO2"=list(abbreviation="MnO2", name="manganese di-oxide", involved_in=list(), activated=TRUE),
  "Mn2+"=list(abbreviation="Mn2+", name="manganese", involved_in=list(), activated=TRUE),
  "MnCO3"=list(abbreviation="MnCO3", name="manganese carbonate", involved_in=list(), activated=TRUE),
  "Fe(OH)3"=list(abbreviation="Fe(OH)3", name="iron hydroxide", involved_in=list(), activated=TRUE),
  "Fe(OH)3PO4"=list(abbreviation="Fe(OH)3PO4", name="iron bound phosphorous", involved_in=list(), activated=TRUE),
  "Fe2+"=list(abbreviation="Fe2+", name="iron", involved_in=list(), activated=TRUE),
  "FeCO3"=list(abbreviation="FeCO3", name="iron carbonate (siderite)", involved_in=list(), activated=TRUE),
  "Fe3(PO4)2"=list(abbreviation="Fe3(PO4)2", name="ferrous phosphate (vivianite)", involved_in=list(), activated=TRUE),
  "FeS"=list(abbreviation="FeS", name="iron mono-sulphide", involved_in=list(), activated=TRUE),
  "FeS2"=list(abbreviation="FeS2", name="iron di-sulphide (pyrite)", involved_in=list(), activated=TRUE),
  "S0"=list(abbreviation="S0", name="elemental sulphur", involved_in=list(), activated=TRUE),
  "SO42-"=list(abbreviation="SO42-", name="sulphate", involved_in=list(), activated=TRUE),
  "H2S"=list(abbreviation="H2S", name="hydrogen sulphate (pool)", involved_in=list(), activated=TRUE),
  "CH4"=list(abbreviation="CH4", name="methane", involved_in=list(), activated=TRUE),
  "DIC"=list(abbreviation="DIC", name="dissolved inorganic carbon", involved_in=list(), activated=TRUE),
  "S0"=list(abbreviation="S0", name="elemental sulphur", involved_in=list(), activated=TRUE)
  )

# define reactions
  # create an list entry in the following form for every reaction considered; reagents and products have to named by defined abbreviation (see section above)
  # E_dummy=list(abbreviation="E_dummy"
  #              ,name="dummy reaction",
  #              reaction_rate=X,
  #              involved_species=list(educts=list("E1", "E2"), products=list("P1", "P2")),
  #              reversibel=FALSE,
  #              activated=TRUE)
  
reactions_collection <- list(
  E1=list(abbreviation="E1",
          name="Aerobic OM mineralisation",
          reaction_rate=NA,
          involved_species=list(educts=list("OM", "O2"), products=list("CO2", "NH4+", "PO4")),
          reversibel=FALSE,
          activated=TRUE),
  E2=list(abbreviation="E2",
          name="Denitrification coupled to OM degradation",
          reaction_rate=NA,
          involved_species=list(educts=list("OM", "NO3-"), products=list("CO2", "NH4+", "PO4", "N2")),
          reversibel=FALSE,
          activated=TRUE),
  E3=list(abbreviation="E3",
          name="MnO2 reduction coupled to OM degradation",
          reaction_rate=NA,
          involved_species=list(educts=list("OM", "MnO2"), products=list("CO2", "NH4+", "PO4", "Mn2+")),
          reversibel=FALSE,
          activated=TRUE),
  E4a=list(abbreviation="E4a",
          name="FeOx reduction coupled to OM degradation",
          reaction_rate=NA,
          involved_species=list(educts=list("OM", "Fe(OH)3"), products=list("CO2", "NH4+", "PO4", "Fe2+")),
          reversibel=FALSE,
          activated=TRUE),
  E4b=list(abbreviation="E4b",
           name="FeOx reduction (using FeP) coupled to OM degradation",
           reaction_rate=NA,
           involved_species=list(educts=list("OM", "Fe(OH)3PO4"), products=list("CO2", "NH4+", "PO4", "Fe2+")),
           reversibel=FALSE,
           activated=TRUE),
  E5=list(abbreviation="E5",
           name="Sulfate reduction coupled to OM degradation",
           reaction_rate=NA,
           involved_species=list(educts=list("OM", "SO42-"), products=list("CO2", "NH4+", "PO4", "H2S")),
           reversibel=FALSE,
           activated=TRUE),
  E6=list(abbreviation="E6",
          name="Methanogenesis",
          reaction_rate=NA,
          involved_species=list(educts=list("OM"), products=list("CO2", "NH4+", "PO4", "CH4")),
          reversibel=FALSE,
          activated=TRUE),
  E8=list(abbreviation="E8",
          name="Nitrification",
          reaction_rate=NA,
          involved_species=list(educts=list("O2", "NH4+", "DIC"), products=list("NO3-", "CO2")),
          reversibel=FALSE,
          activated=TRUE),
  E9a=list(abbreviation="E9a",
          name="Fe(OH)3 formation",
          reaction_rate=NA,
          involved_species=list(educts=list("O2", "Fe2+", "DIC"), products=list("Fe(OH)3")),
          reversibel=FALSE,
          activated=FALSE),
  E9b=list(abbreviation="E9b",
          name="Fe(OH)3PO4 formation",
          reaction_rate=NA,
          involved_species=list(educts=list("O2", "Fe2+", "DIC", "PO4"), products=list("Fe(OH)3PO4", "CO2")),
          reversibel=FALSE,
          activated=TRUE),
  E12=list(abbreviation="E12",
          name="H2S oxidation",
          reaction_rate=NA,
          involved_species=list(educts=list("O2", "H2S", "DIC"), products=list("SO42-", "CO2")),
          reversibel=FALSE,
          activated=TRUE),
  E13=list(abbreviation="E13",
          name="Aerobic methane oxidation",
          reaction_rate=NA,
          involved_species=list(educts=list("O2", "CH4"), products=list("CO2")),
          reversibel=FALSE,
          activated=TRUE),
  E14a1=list(abbreviation="E14a1",
          name="Feoxa-reduction coupled to sulphide oxidation",
          reaction_rate=NA,
          involved_species=list(educts=list("Fe(OH)3", "H2S", "CO2"), products=list("Fe2+", "S0", "DIC")),
          reversibel=FALSE,
          activated=FALSE),
  E14a2=list(abbreviation="E14a2",
             name="FePa-reduction coupled to sulphide oxidation",
             reaction_rate=NA,
             involved_species=list(educts=list("Fe(OH)3PO4", "H2S", "CO2"), products=list("Fe2+", "PO4", "S0", "DIC")),
             reversibel=FALSE,
             activated=TRUE),
  E15=list(abbreviation="E15",
             name="FeS formation",
             reaction_rate=NA,
             involved_species=list(educts=list("Fe2+", "H2S"), products=list("FeS")),
             reversibel=FALSE,
             activated=FALSE),
  E19=list(abbreviation="E19",
             name="SO4-reduction coupled to AOM",
             reaction_rate=NA,
             involved_species=list(educts=list("SO42-", "CH4", "CO2"), products=list("DIC", "H2S")),
             reversibel=FALSE,
             activated=TRUE),
  E27=list(abbreviation="E27",
           name="MnCO3 precipitation",
           reaction_rate=NA,
           involved_species=list(educts=list("Mn2+", "DIC"), products=list("MnCO3")),
           reversibel=FALSE,
           activated=TRUE),
  E28=list(abbreviation="E28",
           name="Mn oxidation",
           reaction_rate=NA,
           involved_species=list(educts=list("Mn2+", "O2"), products=list("MnO2")),
           reversibel=FALSE,
           activated=TRUE),
  E29a1=list(abbreviation="E29a1",
           name="MnO2A reduction coupled to Fe oxidation",
           reaction_rate=NA,
           involved_species=list(educts=list("MnO2", "Fe2+"), products=list("Mn2+", "Fe(OH)3")),
           reversibel=FALSE,
           activated=TRUE),
  E29a2=list(abbreviation="E29a2",
             name="MnO2A reduction coupled to Fe(P) oxidation",
             reaction_rate=NA,
             involved_species=list(educts=list("MnO2", "PO4", "Fe2+"), products=list("Mn2+", "Fe(OH)3PO4")),
             reversibel=FALSE,
             activated=TRUE),
  E30a=list(abbreviation="E30a",
             name="MnO2A reduction coupled to S oxidation",
             reaction_rate=NA,
             involved_species=list(educts=list("MnO2", "H2S"), products=list("Mn2+", "S0")),
             reversibel=FALSE,
             activated=TRUE),
  E32a=list(abbreviation="E32a",
            name="MnO2A-reduction coupled AOM",
            reaction_rate=NA,
            involved_species=list(educts=list("MnO2", "CH4"), products=list("Mn2+", "DIC")),
            reversibel=FALSE,
            activated=TRUE),
  E22=list(abbreviation="E22",
           name="FeCO3 formation",
           reaction_rate=NA,
           involved_species=list(educts=list("Fe2+", "DIC"), products=list("FeCO3")),
           reversibel=FALSE,
           activated=TRUE),
  E33=list(abbreviation="E33",
           name="FeCO3 dissolution",
           reaction_rate=NA,
           involved_species=list(educts=list("FeCO3"), products=list("Fe2+", "DIC")),
           reversibel=FALSE,
           activated=TRUE),
  E24=list(abbreviation="E24",
           name="vivianite formation",
           reaction_rate=NA,
           involved_species=list(educts=list("Fe2+", "PO4"), products=list("Fe3(PO4)2")),
           reversibel=FALSE,
           activated=TRUE),
  E25=list(abbreviation="E25",
           name="vivianite dissolution",
           reaction_rate=NA,
           involved_species=list(educts=list("Fe3(PO4)2"), products=list("Fe2+", "PO4")),
           reversibel=FALSE,
           activated=TRUE),
  E26=list(abbreviation="E26",
           name="Conversion of vivianite to FeS",
           reaction_rate=NA,
           involved_species=list(educts=list("Fe3(PO4)2"), products=list("FeS", "PO4")),
           reversibel=FALSE,
           activated=TRUE),
  E10=list(abbreviation="E10",
           name="FeS dissolution ",
           reaction_rate=NA,
           involved_species=list(educts=list("O2", "FeS"), products=list("SO42-", "Fe2+")),
           reversibel=FALSE,
           activated=FALSE),
  E11=list(abbreviation="E11",
           name="FeS2 dissolution",
           reaction_rate=NA,
           involved_species=list(educts=list("O2", "FeS2"), products=list("SO42-", "Fe2+")),
           reversibel=FALSE,
           activated=TRUE),
  E16=list(abbreviation="E16",
           name="Pyrite formation",
           reaction_rate=NA,
           involved_species=list(educts=list("FeS", "H2S"), products=list("FeS2")),
           reversibel=FALSE,
           activated=TRUE),
  E18=list(abbreviation="E18",
           name="Pyrite formation",
           reaction_rate=NA,
           involved_species=list(educts=list("FeS", "S0"), products=list("FeS2")),
           reversibel=FALSE,
           activated=TRUE),
  E23=list(abbreviation="E23",
           name="Conversion of Fe carb to FeS",
           reaction_rate=NA,
           involved_species=list(educts=list("FeCO3", "H2S"), products=list("FeS", "DIC")),
           reversibel=FALSE,
           activated=TRUE)
  )
  # E17 is missing!!!


###########################################################################################################
#                        DONT CHANGE ANYTHING BELOW HERE WITHOUT INTENDED PURPOSE                         #
###########################################################################################################

# Create list with occuring reactions and based on it list with occuring species.
# Reactions can't occur if at least one educt is missing (required species defined in reactions_collection).
# This can happen, if a species was disabled manually in species_collection (activated=FALSE).
  
# create empty lists
occuring_reactions <- list()
occuring_species <- list()

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

# create "occuring_species"-list
# go through occuring reactions...
for (reaction in occuring_reactions){
  # add involved species to "occuring_species"-list, if it is not in yet and not manually deactivated
  for (species in c(reaction$involved_species$educts, reaction$involved_species$products)){
    if ((!exists(species, occuring_species))&(species_collection[[species]]$activated==TRUE)){
      occuring_species <- c(occuring_species, species_collection[species])
    }
  }
}

# adjust "occuring_species$involved_in"-list and "occuring_reactions$involved_species"-list
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

# clear workspace: remove auxiliary variables and collections
rm(reactions_collection, species_collection, reaction, species, checkup, i, number, type, typen)
