
###########################################################################################################
#                                             MODEL PREPERATION                                           #
###########################################################################################################

# create lists that store rate-equation-functions and rate-constants based on "occuring_reactions"-list, conversion factors, "total concentration change terms" RX
create_model_lists <- function(){
  # source files
  source(file="01_parameters_config.R", local=TRUE)
  source(file="021_chemical_base_config.R", local=TRUE)
  
  # table of contents / create variables
  species_operational <<- list() # 1) operational species list: "subspecies" will be assigned as own species for the further procedure
  state <<- c() # 2.1) state vector (needed as function argument)
  names_out <<- c() # 2.2) used to label steady-state output (in ode.1D it is only used for plotting)
  
  transport_terms <<- list() # 4) store tranX-terms as text
  rate_constants <<- list() # 5.1) list of all reaction rate constants in "occuring_reactions"-list
  rate_equations <<- list() # 5.2) list of all reaction rate equations in "occuring_reactions"-list
  shared_reg_terms <<- list() # 5.3) assign "shared_reaction_terms" from "chemical_base_config" to global environment
  reaction_terms <<- list() # 5.4) calculate change of a species concentration through chemical processes by summing up the reaction rates of reactions this species is involved in
  total_c_change <<- list() # 6) "total concentration change terms": transport term + reaction term for each species (dXdt = tranX$dC + RX)
  returnlist <<- "" # 7) list of content that Model function returns, formulated as text
  # 8) assign "N" from "chemical_base_config" to global environment
  
  
  
  # 1) create operational species list
  for (species in occuring_species){
    # if there are subspecies ...
    if (length(species$subspecies) > 0) {
      # ... create new species-entries for each subspecies; copy information from "mother-species" for further processing.
      for (i in seq_along(species$subspecies)){
        species_operational[[species$subspecies[[i]]]] <<- c(species["abbreviation"], species["involved_in"], species["phase"], name = species$subspecies[[i]], number=i) 
        #"number" stored to match the right reaction rate later on
        # "abbreviation" stored to get information out of "occuring_reactions"-list (reference to "mother-species")
      }
    }
    else{
      # if there is no subspecies: copy entry partly from "occuring_species"-list to "species_operational"-list
      species_operational[[species$abbreviation]] <<- c(species["abbreviation"], species["involved_in"], species["phase"], name = species$abbreviation)
    }
  }
  
  
  
  # 2.1) & 2.2)
  for (species in species_operational){
    # 2.1) initial state-vector (input for steady state solving)
    state <<- c(state, rep(0, length.out = N))
    # 2.2) names-vector (species have to be in same order as in state vector)
    names_out <<- c(names_out, species$name)
  }
  
  
  
  # 4) define transport terms
  for (species in species_operational) {
    # transport terms differ for solids and solutes
    if (species$phase == "solute") {
      # solutes
      # varaibles name
      var_name <- paste("tran", species$name, sep = "")
      # varaibles content
      var_content <- paste("tran.1D(C=", species$name, ", C.up=", species$name, "_top, D=grid_collection$D", species$name, ".grid, v=grid_collection$u.grid, VF=grid_collection$por.grid, dx=grid_collection$grid)",  sep = "")
      # store tranX-term as text in "transpor_terms"-list ...
      transport_terms[[var_name]] <<- var_content
    }
    else if (species$phase == "solid") {
      # solids
      # varaibles name
      var_name <- paste("tran", species$name, sep = "")
      # varaibles content
      var_content <- paste("tran.1D(C=", species$name, ", flux.up=F_", species$name, ", D=grid_collection$Db.grid", ", v=grid_collection$v.grid, VF=grid_collection$svf.grid, dx=grid_collection$grid)",  sep = "")
      # store tranX-term as text in "transpor_terms"-list ...
      transport_terms[[var_name]] <<- var_content
    }
  }
  
  
  
  # 5.1) & 5.2)
  # go through "occuring_reactions"-list
  for (element in occuring_reactions){
    
    # 5.1) extract reaction rate constants
    for (i in seq_along(element$reaction_rate_constants)){
      rate_constants[names(element$reaction_rate_constants[i])] <<- element$reaction_rate_constants[[i]]$value
    }
    
    # 5.2) extract rate equations for every reaction ...
    temp_list <- c(element$reaction_rates$equations)
    # ... and add them to "rate-quations"-list
    rate_equations <<- c(rate_equations, temp_list)
  }
  
  
  # 5.3) get "shared_regulation terms from "chemical_base_config"
  shared_reg_terms <<- shared_regulation_terms
  
    
  # 5.4) define reaction terms: build RX-term for each species
  
  # Conversion factors: needed for reaction terms
  q <- mean(grid_collection$svf.grid$mid / grid_collection$por.grid$mid) # from 1/svf to 1/por; solid to aqeaous
  r <- mean(grid_collection$por.grid$mid / grid_collection$svf.grid$mid) # from 1/por to 1/svf; aqeous to solid
  
  for (species in species_operational){
    
    # store name for reaction-term variable
    var_name <- paste("R", species$name, sep = "")
    # prepare "var_content"-variable
    var_content <- ""
    
    # go through reactions the species is involved in
    for (element in species$involved_in){
      # check some properties and store them in helping variables to build RX-term later
      
      # is species educt in this reaction? -> True: minus-sign, False: plus-sign
      educt <- exists(species$abbreviation, occuring_reactions[[element]]$involved_species$educts)
      
      # get stoichiometry-factor out of "occuring_reactions"-list
      # differenciate between educts and products because of the "path"
      if (educt){
        stoic <- occuring_reactions[[element]]$involved_species$educts[[species$abbreviation]]$stoic
      }
      else {
        stoic <- occuring_reactions[[element]]$involved_species$products[[species$abbreviation]]$stoic
      }
      
      # check if/which conversion factor is needed
      # if species is solute and unit of reaction rate is "mol/V_sf/y" -> q; if species is solid and unit of reaction rate is "mol/V_pw/y" -> r
      if ((species$phase=="solute")&(occuring_reactions[[element]]$reaction_rates$u_unit=="mol/V_sf/y")) conversion <- q
      else if ((species$phase=="solid")&(occuring_reactions[[element]]$reaction_rates$u_unit=="mol/V_pw/y")) conversion <- r
      else conversion <- 1
      
      # get reaction rate name(s)
      # does reaction occur with two different rates, depending on this species? (e.g. different degrees of degradability of organic matter)
      if ((length(occuring_reactions[[element]]$varying_rates) == 0) || (occuring_reactions[[element]]$varying_rates != species$abbreviation)){
        # yes: either there is no differentiation of reaction rates for this reaction or it is not differentiated "for this species"
        # get reaction rate name(s) (more rates for different degrees of degradability, but not created for this species)
        # and store them in "reaction_rates"-string, connected by plus-sign, in form of a function call
        reaction_rates <- ""
        for (i in seq_along(occuring_reactions[[element]]$reaction_rates$equations)){
          rr_temp <- paste("+", names(occuring_reactions[[element]]$reaction_rates$equations[i]), sep = "")
          reaction_rates <- paste(reaction_rates, rr_temp)
        }
      }
      else if (occuring_reactions[[element]]$varying_rates == species$abbreviation){
        # yes: there are different reaction rate equations stored in the reaction description; the different rates were distinguished for the current species, among others
        # get reaction rate name (the one that fits the current subspecies; this is estimated by the "number" element, stored for "subspecies" in the species_operational"-list)...
        # ...and store it in "reaction_rate"-string in form of a function call
        reaction_rates <- names(occuring_reactions[[element]]$reaction_rates$equations[species$number])
      }
      
      # create summand for this reaction ...
      content_part <- paste(ifelse(educt, "-", "+"), conversion, "*", stoic, "*", "(", reaction_rates, ")", sep="")
      # ... and add it to "var-content"-variable
      var_content <- paste(var_content, content_part, sep="")
    }

    # store RX-terms as text in "reaction_terms"-list
    reaction_terms[[var_name]] <<- var_content
  }
  
  
  
  # 6) define "total concentration change terms"
  for (species in species_operational){
    # store name for variable
    var_name <- paste("d", species$name, "dt", sep = "")
    # "var_content"-variable
    var_content <- paste("tran", species$name, "$dC + R", species$name, sep = "")
    # store dXdt-term as text in "total_c_change"-list
    total_c_change[[var_name]] <<- var_content
  }
  
  
  
  # 7) define returnlist
    # The return value of func should be a list, whose first element is a vector containing the derivatives of y with respect to time,
    # and whose next elements are global values whose steady-state value is also required. (R Documentation, steady.1D)
  #***returnlist <- "list(c(b=b, c=c), a=a, b=b)"
  
  # function to create ennumeration strings out of a list in the form "name1=name1, name2=name2" 
  list_to_ennumeration <- function(source_list){
    for (i in seq_along(source_list)){
      new_entry <- paste(names(source_list[i]), "=", names(source_list[i]), sep = "")
      if (i == 1){
        ennumeration <- new_entry
      }
      else {
        ennumeration <- paste(ennumeration, new_entry, sep = ", ") 
      }
    }
    return(ennumeration)
  }
  
  # create "vector containing the derivatives of y (state) with respect to time", which are the "total concentration change terms" in our case
  ennumeration <- list_to_ennumeration(total_c_change)
  result_vec <- paste("c(", ennumeration, ")", sep = "")
  rm(ennumeration)
  
  # create list of "next elements whose steady-state value are also required" (reaction rates, shared reaction terms, reaction terms, fluxes at upper and lower boundary)
    # everything except the upper and lower boundary content can be done with "list to ennumeration"-function
    ennumeration_1 <- list_to_ennumeration(c(rate_equations, shared_reg_terms, reaction_terms))
    
    # fluxes at upper and lower boundary (FU_X=tranX$flux.up and FD_X=tranX$flux.down for each species X)
    fluxes_ennumeration <- function(){
      for (i in seq_along(species_operational)){
        name <- names(species_operational[i])
        new_entry <- paste("FU_", name, "=(tran", name, "$flux.up), FD_", name, "=(tran", name, "$flux.down)", sep = "")
        if (i == 1){
          ennumeration <- new_entry
        }
        else {
          ennumeration <- paste(ennumeration, new_entry, sep = ", ") 
        }
      }
      return(ennumeration)
    }
    ennumeration_2 <- fluxes_ennumeration()
    
  # create returnlist (to be evaluated in model function)
  returnlist <<- paste("list(", result_vec, ", ", ennumeration_1, ", ", ennumeration_2, ")", sep = "")

  
  
  # 8) assign "N" from "chemical_base_config" to global environment
  N <<- N
}

create_model_lists()

#***************************
# clean_up
#***************************
# clear workspace: remove function
rm(create_model_lists)
