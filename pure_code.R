## ---- libraries ---------------------------------------------------------------------------------
library(shiny)
library(DT)
library(visNetwork)
library(ggplot2)
library(stringr)
library(shape)
library(deSolve)
library(rootSolve)
library(ReacTran)
library(marelac)


## ---- preparations ------------------------------------------------------------------------------
handlers <- list()


## ---- set_configs -------------------------------------------------------------------------------
configs <- list(
  parameters_config = "parameters_config.R",
  chemical_base_config = "chemical_base_config.R",
  boundary_conditions_config = "boundary_conditions_config.R" 
)


## ---- source_parameters -------------------------------------------------------------------------
handlers$parmslist <- function(){
  # source parameters_configuration file
  source(file=configs$parameters_config, local=TRUE)
  
  #create time sequence
  times <- seq(0, tmax, by = tint)
  
  # create temperature function
  # "extrapolate" loaded one-year time series to get complete year cycle in spline
  temp.time <- c(temp.data[["time"]]-1, temp.data[["time"]], temp.data[["time"]]+1)
  temp.value <- rep(temp.data[["temperature"]], 3)
  # evaluate the spline
  temp.spline <- smooth.spline(temp.time, temp.value)
  # approximate a function with spline values
  TC_func <- approxfun(temp.spline)
  # # check spline
  # plot(temp.value ~ temp.time)
  # lines(temp.spline, col="blue")
  
  # pack needed parameters from configuration file to list
  parameters <- list(
    L = L,
    N = N,
    
    por_shape = por_shape,
    por.0 = por.0,
    por.inf = por.inf,
    
    dens_dw = dens_dw,
    S = S,
    P = P,
    v = v,
    Db = Db,
    Db_depth = Db_depth,
    
    CtoN = CtoN,
    CtoP = CtoP,
    
    times = times,
    TC_func = TC_func
  )
  
  return(parameters)
}
parameters <- handlers$parmslist()


## ---- chemical_lists ----------------------------------------------------------------------------
handlers$chemical_base_main <- function(specify=FALSE, reaction_list){
  # load parameters to function environment
  list2env(parameters, envir = rlang::current_env())
  # source chemical configuration
  source(file=configs$chemical_base_config, local=TRUE)
  
  # use this arguments to create model with only explicitly named reactions
  # e.g. chemical_base_main(specify=TRUE, list("E1", "E9b"))
  if (specify ==TRUE) {
    for (reaction in reactions_collection){
      if (any(reaction_list==reaction$abbreviation)){
        reactions_collection[[reaction$abbreviation]]$activated <- TRUE
      }
      else {
        reactions_collection[[reaction$abbreviation]]$activated <- FALSE
      }
    }
  }
  
  # name sublists of "collection"-lists
  for (i in seq_along(species_collection)){
    names(species_collection)[i] <- species_collection[[i]]$abbreviation
  }
  for (i in seq_along(reactions_collection)){
    names(reactions_collection)[i] <- reactions_collection[[i]]$abbreviation
  }
  
  # create "occurring_reactions"-list: list with occurring reactions based on activated reactions in "chemical_base_config"-script
  occurring_reactions <- list()
  for (reaction in reactions_collection){
    # check if reaction is activated
    if (reaction$activated==TRUE){
        # add reaction to occurring_reactions-list...
        occurring_reactions <- c(occurring_reactions, list(reaction))
        # ...and set reaction name to make occurring_reactions-data clearer
        names(occurring_reactions)[length(occurring_reactions)] <- reaction$abbreviation
    }
  }
  
  # create "occurring_species"-list: list with occurring species based on occurring-reactions list
  occurring_species <- list()
  for (reaction in occurring_reactions){
    # add involved species to "occurring_species"-list, if it is not in yet
    for (species in c(reaction$involved_species$educts, reaction$involved_species$products)){
      if (!exists(species$abbreviation, occurring_species)){
        occurring_species <- c(occurring_species, species_collection[species$abbreviation])
      }
    }
  }

  # add occurring reactions to "occurring_species$involved_in"-list
  for (reaction in occurring_reactions){
    for (species in c(reaction$involved_species$educts, reaction$involved_species$products)){
        occurring_species[[species$abbreviation]]$involved_in <- c(occurring_species[[species$abbreviation]]$involved_in, reaction$abbreviation)
    }
  }
  
  return(list(oc_reactions=occurring_reactions, oc_species=occurring_species, col_reactions=reactions_collection))
}
chemical_lists <- handlers$chemical_base_main()
occurring_reactions <- chemical_lists$oc_reactions
occurring_species <- chemical_lists$oc_species
reactions_collection <- chemical_lists$col_reactions
rm(chemical_lists)


## ---- interactive_diagram -----------------------------------------------------------------------
handlers$create_diagram <- function(){
  # two data-frames ("nodes" and "links") are created, based on the "occurring_reactions"-list
  # these data-frames are needed to draw a diagram using the visNetwork-package
  
  # create empty data frames
  nodes <- data.frame(id=c(), label=c(), title=c(), group=c(), name=c())
  links <- data.frame(from=c(), to=c(), title=c())
  
  # fill data frames
  # go through occurring_reactions
  for (reaction in occurring_reactions){
    # create and attach "reaction node"
    # get rate expressions
    rate_expr <- ""
    for (i in seq_along(reaction$reaction_rates$equations)){
      rate_expr <- paste(rate_expr, paste("<p>", names(reaction$reaction_rates$equations)[i], "=", reaction$reaction_rates$equations[i]), "</p>")
    }
    new_node <- data.frame(id=c(reaction$abbreviation), label=c(""), title=c(paste(reaction$name, rate_expr)), group=c("reaction"), name=c(reaction$name))
    nodes <- rbind(nodes, new_node)
    
    # create and attach nodes for species and links between species- and reaction-nodes
    for (species in c(reaction$involved_species$educts, reaction$involved_species$products)){
      # new "species-node", if it does not exist yet
      abbr <- species$abbreviation
      if (any(nodes==abbr)==FALSE){
        new_node <- data.frame(id=c(abbr), label=c(abbr), title=c(abbr), group=c("species"), name=c(abbr))
        nodes <- rbind(nodes, new_node)
      }
      # new link(s)
      # check if current species is educt
      if (exists(abbr, reaction$involved_species$educts)){
        # create new link: species -> reaction
        new_link <- data.frame(from=c(abbr), to=c(reaction$abbreviation), title = species$stoic)
        links <- rbind(links, new_link)
      }
      else{
        # create link: reaction -> species
        new_link <- data.frame(from=c(reaction$abbreviation), to=c(abbr), title = species$stoic)
        links <- rbind(links, new_link)
      }
    }
  }
  
  # order "nodes"-data-frame by groups -> selection-menu in diagram will be structured
  nodes <- nodes[order(nodes$group), ]
  
  # create visNetwork object
  model_diagram <- visNetwork(nodes, links)
  
  # set some general options
  model_diagram <- visOptions(model_diagram,
                              width = 1920, height = 1080,
                              highlightNearest = list(enabled = TRUE, degree = list(from = 2, to = 2), algorithm = "hierarchical"),
                              selectedBy = list(variable = "name", selected = "OM", highlight = TRUE, sort = FALSE)
  )
  
  # set diagram layout (igraph layout)
  # there are many options, e.g.: layout_nicely; _with_dh; with_gem; _as_tree (if you have a "non circled" setup); _with_lgl; merge_coords; normalize
  model_diagram <- visIgraphLayout(model_diagram, layout = "layout_with_dh")
  
  # node appeariance adjustment per group
  # reactions
  model_diagram <- visGroups(model_diagram, groupname = "reaction",
                             shape = "dot",
                             size = 5,
                             color = list(background = "lightblue", border="lightblue", highlight = list(background = "orange", border = "orange")),
                             shapeProperties = list(borderDashes = c(10,5)),
                             hidden=FALSE)
  # species
  model_diagram <- visGroups(model_diagram, groupname = "species",
                             shape = "box",
                             color = list(background = "white", border="royalblue", highlight = list(background = "orange", border = "darkred")),
                             shapeProperties = list(borderDashes = FALSE))
  
  # global links customisation
  model_diagram <- visEdges(model_diagram,
                            arrows = "to",
                            color = list(color = "royalblue", highlight = "darkred"),
                            smooth = TRUE)
  
  return(model_diagram)
}
model_diagram <- handlers$create_diagram()


## ---- grid_setup --------------------------------------------------------------------------------
# create grid and attach parameters to grid
handlers$grid_setup <- function(){
  # load environmental and grid parameters to function environment
  list2env(parameters, envir = rlang::current_env())
  
  # create "grid_collection"-list: stores grid and grid properties
  grid_collection <- list()
  
  # setup grid
  grid <- setup.grid.1D(x.up = 0, L = L, N = N)
  #grid <- setup.grid.1D(x.up = 0, L = 0.3, N = 30, p.dx.1 = 20, dx.1 = 0.0002) # temporary higher resolution at SWI to fit O2 data
  
  # attach bioturbation-coefficient
  #Db.grid <- setup.prop.1D(value = Db, grid = grid)
  Db.grid <- setup.prop.1D(func=p.sig, grid = grid, y.0 = Db, y.inf = 0,x.L = Db_depth, x.att = 0.05) # Db=0, no bioturbation
  
  # attach porosity
  por.grid <- setup.prop.1D(func=p.exp, grid=grid, y.0 = por.0, y.inf = por.inf, x.att = por_shape)
  
  # attach solid volume fraction
  svf.grid <-setup.prop.1D(func=p.exp, grid=grid, y.0 = 1-por.0, y.inf = 1-por.inf, x.att = por_shape)
  
  # attach diffusive parameters for advection at top
  #now deliver the same values as porosity does not change with depth; to check correct OM degradation sequences
  #if porosity changes with depth then it will vary
  dummy <- setup.compaction.1D(v.0 = v, por.0=por.0, por.inf=por.inf, por.grid=por.grid)
  v.grid <- dummy$v   # solid phase advective velocities
  u.grid <- dummy$u   # dissolved phase advective velocities
  
  
  # temperature-dependent diffusion coefficients for solute species:
  # a) dummy variables in "grid_collection"-list (DX.grid),
  # b) preparations and function to calculate values in model-function (solute_diffusion_coffs(TC)), because Dmol.X is temperature-dependent
  # approach:
  # diffusion coefficient for solutes: DX = Dmol.X + Db, where Dmol.X is the molecular diffusion coefficient calculated by diffcoeff-function of the marelac package ...
  # ... which is also corrected for tortuosity and adjusted for our time unit
  # Dmol.X <- diffcoeff(S = S, t = TC, P = P, species = name_diffcoeff)[[name_diffcoeff]] * sectoyr/tort 
  # tort <- 1 - 2*log(por.0)  correction for tortuosity
  # sectoyr <- 3600*24*365.25  conversion from s to yr; diffcoeff returns ionic diffusion coefficients in m2/s
  
  # a) set dummy-grid-properties for solute species
  for (i in seq_along(occurring_species)){
    if (occurring_species[[i]]$phase == "solute"){
      # construct "Dx.grid" variable name, e.g. DO2.grid
      name <- occurring_species[[i]]$abbreviation # species name
      name <- paste("D", name, ".grid", sep = "") # variable name
      # create dummy-grid-property DX.grid ...
      DX.grid <- setup.prop.1D(value = "dummy",  grid = grid)
      # ... and attach it to "grid_collection"-list
      grid_collection[[name]] <- DX.grid
    }
  }
  # b)
  solutes <- list()
  solutes$names <- c()
  solutes$D_names <- c()
  for (i in occurring_species){
    if (i$phase =="solute"){
      solutes$names <- c(solutes$names, i$abbr_diffcoeff)
      solutes$D_names <-  c(solutes$D_names, paste("D", i$abbreviation, ".grid", sep = ""))
    }
  }
  solutes$corr <- list()
  solutes$corr$mid <- 3600*24*365.25/(1 - 2*log(por.grid$mid))
  solutes$corr$int <- 3600*24*365.25/(1 - 2*log(por.grid$int))
  
  solutes$func <- function(name, Dmol.X, corr=grid_collection$solutes$corr, Db=grid_collection$Db.grid){
    temp <- mapply(FUN = function(Dmol.X, corr, Db){Dmol.X * corr + Db}, Dmol.X, corr, Db)
    grid_collection[[name]]$mid <<- temp[[1]]
    grid_collection[[name]]$int <<- temp[[2]] 
  }
  environment(solutes$func) <- parent.frame()

  # put it all together
  grid_collection <- c(grid_collection, list(grid=grid, Db.grid=Db.grid, por.grid=por.grid, svf.grid=svf.grid, v.grid=v.grid, u.grid=u.grid, solutes=solutes))
  return(grid_collection)
}

grid_collection <- handlers$grid_setup()


## ---- boundary_conditions -----------------------------------------------------------------------
handlers$get_boundaries <- function(){
  source(file=configs$boundary_conditions_config, local=TRUE)
  return(boundary_conditions)
}
boundary_conditions <- handlers$get_boundaries()


## ---- list_preparation --------------------------------------------------------------------------
handlers$create_model_lists <- function(){
  list2env(parameters, envir = rlang::current_env())
  source(file=configs$chemical_base_config, local=TRUE)
  
  # table of contents / create variables
  species_operational <- list() # 1) operational species list: "subspecies" will be assigned as own species for the further procedure
  state <- c() # 2.1) state vector (needed as function argument)
  names_out <- c() # 2.2) used to label steady-state output (in ode.1D it is only used for plotting)
  transport_terms <- list() # 4) store tranX-terms as text
  rate_constants <- list() # 5.1) list of all reaction rate constants in "shared_reaction_constants"-list and occurring_reactions"-list
  rate_equations <- list() # 5.2) list of all reaction rate equations in "occurring_reactions"-list
  shared_reg_terms <- list() # 5.3) assign "shared_reaction_terms" from "chemical_base_config" to global environment
  reaction_terms <- list() # 5.4) calculate change of a species concentration through chemical processes by summing up the reaction rates of reactions this species is involved in
  total_c_change <- list() # 6) "total concentration change terms": transport term + reaction term for each species (dXdt = tranX$dC + RX)
  returnlist <- "" # 7) list of content that Model function returns, formulated as text
  expressions <- list() # 8) list of expressions to be evaluated in the model function
  
  
  
  # 1) create operational species list
  for (species in occurring_species){
    # if there are subspecies ...
    if (length(species$subspecies) > 0) {
      # ... create new species-entries for each subspecies; copy information from "mother-species" for further processing.
      for (i in seq_along(species$subspecies)){
        species_operational[[species$subspecies[[i]]]] <- c(species["abbreviation"], if("abbr_diffcoeff" %in% names(species)){species["abbr_diffcoeff"]}, species["involved_in"], species["phase"], name = species$subspecies[[i]], subsp_tag = names(species$subspecies)[i]) 
        #"number" stored to match the right reaction rate later on
        # "abbreviation" stored to get information out of "occurring_reactions"-list (reference to "mother-species")
      }
    }
    else{
      # if there is no subspecies: copy entry partly from "occurring_species"-list to "species_operational"-list
      species_operational[[species$abbreviation]] <- c(species["abbreviation"], if("abbr_diffcoeff" %in% names(species)){species["abbr_diffcoeff"]}, species["involved_in"], species["phase"], name = species$abbreviation)
    }
  }
  
  
  
  # 2.1) & 2.2)
  for (species in species_operational){
    # 2.1) initial state-vector (input for steady state solving)
    state <- c(state, rep(0, length.out = N))
    # 2.2) names-vector (species have to be in same order as in state vector)
    names_out <- c(names_out, species$name)
  }
  
  
  
  # 4) define transport terms
  for (species in species_operational) {
    # varaibles name
    var_name <- paste("tran", species$name, sep = "")
    # transport terms differ for solids and solutes in general; also there is a special transport term for adsorbed phosphate
    if (species$name == "adsorbed_P") {
      # varaibles content:
      # adsorbed_P is trasportated with FeOH3A, so FeOH3A's concentration and fluxes are used and corrected for the phosphate load
      # if FeOH3A fluxes in the sediment at the upper boundary it is assumed, that no adsorbed phosphate comes in with it
      # if FeOH3A fluxes in the sediment at the lower boundary it is assumed, that the incoming FeOH3 has a phosphate load equal the bottom sediment layer
      # adsorbed phosphate can flux out of the sediment together with FeOH3A on both boundarys with phosphate load at top/bottom layer
      #var_content <- "tran.1D(C=FeOH3A*phosphate_load_FeOH3A, flux.up=ifelse(tranFeOH3A$flux.up<0, tranFeOH3A$flux.up*phosphate_load_FeOH3A[1], 0), flux.down=ifelse(tranFeOH3A$flux.down>0, tranFeOH3A$flux.down*phosphate_load_FeOH3A, 0), D=grid_collection$Db.grid, v=grid_collection$v.grid, VF=grid_collection$svf.grid, dx=grid_collection$grid)"
      var_content <- "tran.1D(C=FeOH3A*phosphate_load_FeOH3A, flux.up=ifelse(tranFeOH3A$flux.up<0, tranFeOH3A$flux.up*phosphate_load_FeOH3A[1], 0), flux.down=tranFeOH3A$flux.down*phosphate_load_FeOH3A[N], D=grid_collection$Db.grid, v=grid_collection$v.grid, VF=grid_collection$svf.grid, dx=grid_collection$grid)"
      # store tranX-term as text in "transpor_terms"-list ...
      transport_terms[[var_name]] <- var_content
    }
    else if (species$phase == "solute") {
      # varaibles content
      var_content <- paste("tran.1D(C=", species$name, ", C.up=", species$name, "_top, D=grid_collection$D", species$name, ".grid, v=grid_collection$u.grid, VF=grid_collection$por.grid, dx=grid_collection$grid)",  sep = "")
      # store tranX-term as text in "transpor_terms"-list ...
      transport_terms[[var_name]] <- var_content
    }
    else if (species$phase == "solid") {
      # varaibles content
      var_content <- paste("tran.1D(C=", species$name, ", flux.up=F_", species$name, ", D=grid_collection$Db.grid", ", v=grid_collection$v.grid, VF=grid_collection$svf.grid, dx=grid_collection$grid)",  sep = "")
      # store tranX-term as text in "transpor_terms"-list ...
      transport_terms[[var_name]] <- var_content
    }
  }
  
  
  # 5.1.1) get "shared_reaction_constants" from "chemical_base_config; store in "rate_constants"-list
  for (i in seq_along(shared_reaction_constants)){
    rate_constants[names(shared_reaction_constants[i])] <- shared_reaction_constants[[i]]$value
  }
  
  
  # 5.1.2,  5.2 & 5.3)
  # go through "occurring_reactions"-list
  for (element in occurring_reactions){
    
    # 5.1.2) extract reaction rate constants that are only used in one reaction
    for (i in seq_along(element$reaction_rate_constants)){
      rate_constants[names(element$reaction_rate_constants[i])] <- element$reaction_rate_constants[[i]]$value
    }
    
    # 5.2) extract rate equations for every reaction ...
    temp_list <- c(element$reaction_rates$equations)
    # ... and add them to "rate-equations"-list
    rate_equations <- c(rate_equations, temp_list)
    
    # 5.3) get "shared_regulation terms, that are actually needed
    for (termname in element$shared_terms){
      shared_reg_terms[termname] <- shared_regulation_terms[[termname]]
    }
  }
  
  
  # 5.4) define reaction terms: build RX-term for each species ("summed production rate")
  
  # store conversion factors as text in "reaction_terms"-list 
  reaction_terms[["q"]] <- grid_collection$svf.grid$mid / grid_collection$por.grid$mid # from 1/svf to 1/por; solid to aqueous
  reaction_terms[["r"]] <- grid_collection$por.grid$mid / grid_collection$svf.grid$mid # from 1/por to 1/svf; aqueous to solid
  
  for (species in species_operational){
    
    # store name for reaction-term variable
    var_name <- paste("R", species$name, sep = "")
    # prepare "var_content"-variable
    var_content <- ""
    
    # go through reactions the species is involved in
    for (element in species$involved_in){
      temp_reaction <- occurring_reactions[[element]]
      # check some properties and store them in helping variables to build RX-term later
      
      # is species educt in this reaction? -> True: minus-sign, False: plus-sign
      educt <- exists(species$abbreviation, temp_reaction$involved_species$educts)
      
      # get stoichiometry-factor out of "occurring_reactions"-list
      # differenciate between educts and products because of the "path"
      if (educt){
        stoic <- temp_reaction$involved_species$educts[[species$abbreviation]]$stoic
      }
      else {
        stoic <- temp_reaction$involved_species$products[[species$abbreviation]]$stoic
      }
      
      # check if/which conversion factor is needed
      # if species is solute and unit of reaction rate is "mol/V_sf/y" -> q; if species is solid and unit of reaction rate is "mol/V_pw/y" -> r
      if ((species$phase=="solute")&(temp_reaction$reaction_rates$u_unit=="mol/V_sf/y")) conversion <- "q"
      else if ((species$phase=="solid")&(temp_reaction$reaction_rates$u_unit=="mol/V_pw/y")) conversion <- "r"
      else conversion <- 1
      
      # get reaction rate name(s)
      # does reaction occur with two different rates, depending on this species? (e.g. different degrees of degradability of organic matter)
      if ((length(temp_reaction$subsp_def) == 0) || (! species$abbreviation %in% temp_reaction$subsp_def)){
        # yes: either there is no differentiation of reaction rates for this reaction or it is not differentiated "for this species"
        # get reaction rate name(s) (more rates for different degrees of degradability, but not created for this species)
        # and store them in "reaction_rates"-string, connected by plus-sign, in form of a function call
        reaction_rates <- ""
        for (i in seq_along(temp_reaction$reaction_rates$equations)){
          rr_temp <- paste("+", names(temp_reaction$reaction_rates$equations[i]), sep = "")
          reaction_rates <- paste(reaction_rates, rr_temp)
        }
      }
      else if (species$abbreviation %in% temp_reaction$subsp_def){
        # yes: there are different reaction rate equations stored in the reaction description; the different rates were distinguished for the current species, among others
        # get reaction rate names (the one that fits the current subspecies; the selection is based on the naming of the subspecies (a, b, c,...) in the species description and the reaction rate name
        # get length of "subsp_def"-vector
        vr_length <- length(temp_reaction$subsp_def)
        # get species position in "subsp_def"-vector: indicates position in reaction rate name to match rates
        position <- which(temp_reaction$subsp_def == species$abbreviation)
        # get substring indicating the involved subspecies in general
        temp_substr <- substr(names(temp_reaction$reaction_rates$equations), (str_length(names(temp_reaction$reaction_rates$equations))-vr_length+1), str_length(names(temp_reaction$reaction_rates$equations)))
        # get substring for species of interest
        temp_substr <- substr(temp_substr, position, position)
        # find match: a, b, c in substring should match a, b, c declaration of subspecies in species description (subsp_tag)
        reaction_selection <- temp_substr == species$subsp_tag
        # store determined reactions in "reaction_rates"-string
        reaction_names <- temp_reaction$reaction_rates$equations[reaction_selection]
        reaction_rates <- ""
        # if there was no reaction determined (e.g. OrgCC) -> reaction rate is 0
        if (length(reaction_names) == 0){
          reaction_rates <- "0"
        } else {
          for (i in seq_along(reaction_names)){
            rr_temp <- paste("+", names(reaction_names[i]), sep = "")
            reaction_rates <- paste(reaction_rates, rr_temp)
          }
        }
      }
      
      # create summand for this reaction ...
      content_part <- paste(ifelse(educt, "-", "+"), conversion, "*", stoic, "*", "(", reaction_rates, ")", sep="")
      # ... and add it to "var-content"-variable
      var_content <- paste(var_content, content_part, sep="")
    }
    
    # store RX-terms as text in "reaction_terms"-list
    reaction_terms[[var_name]] <- var_content
  }
  
  
  
  # 6) define "total concentration change terms"
  for (species in species_operational){
    # store name for variable
    var_name <- paste("d", species$name, "dt", sep = "")
    # "var_content"-variable
    var_content <- paste("tran", species$name, "$dC + R", species$name, sep = "")
    # store dXdt-term as text in "total_c_change"-list
    total_c_change[[var_name]] <- var_content
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
  
  # change in concentration through transport processes (tranX$dC=tranX$dC for each species X)
  dC_ennumeration <- function(){
    for (i in seq_along(species_operational)){
      name <- names(species_operational[i])
      new_entry <- paste("trandC_", name, "=(tran", name, "$dC)", sep = "")
      if (i == 1){
        ennumeration <- new_entry
      }
      else {
        ennumeration <- paste(ennumeration, new_entry, sep = ", ") 
      }
    }
    return(ennumeration)
  }
  ennumeration_3 <- dC_ennumeration()
  
  # create returnlist (to be evaluated in model function)
  returnlist <- paste("list(", result_vec, ", ", ennumeration_1, ", ", ennumeration_2, ", ", ennumeration_3, ")", sep = "")
  
  
  # 8) create evaluatable expressions; wrap them up to one big expression at the end
  # list to string: for rate constants, shared regulation terms, rate equations, reaction terms, 
  list_to_string <- function(mylist){
    result <- c()
    for (i in seq_along(mylist)){
      result <- c(result, parse(text=paste(names(mylist)[i], "<-", mylist[i], ";")))
    }
    return(result)
  }
  # list to string for varying boundary_ conditions
  boundary_expr <- function(){
    mylist <- boundary_conditions$varying
    result <- c()
    for (i in seq_along(mylist)){
      result <- c(result, parse(text=paste(names(mylist)[i], " <- ", "boundary_conditions$varying$", names(mylist[i]), "(t%%1)", sep="")))
    }
    return(result)
  }
  
  expressions <- list()
  expressions$N <- expression(N <- parameters$N)
  expressions$state <- expression(  for (i in seq_along(model_lists$species_operational)){assign(names(model_lists$species_operational[i]), state[((i-1)*N+1):(i*N)])})
  expressions$TC <- expression(TC <- parameters$TC_func(t%%1))
  expressions$rate_constants <- list_to_string(rate_constants)
  expressions$shared_reg_terms <- list_to_string(shared_reg_terms)
  expressions$rate_equations <- list_to_string(rate_equations)
  expressions$reaction_terms <- list_to_string(reaction_terms)
  expressions$solute_diffcoeffs <- expression(Dmols <- as.list(diffcoeff(S = parameters$S, t = TC, P = parameters$P, species = grid_collection$solutes$names)),
                                              mapply(FUN=grid_collection$solutes$func, name=grid_collection$solutes$D_names, Dmol.X=Dmols))
  expressions$bounds_c <- list_to_string(boundary_conditions$constant)
  expressions$bounds_v <- boundary_expr()
  expressions$transport_terms <- list_to_string(transport_terms)
  expressions$total_c_change <- list_to_string(total_c_change)
  expressions$return <- parse(text=paste("return(", returnlist, ")", sep=""))
  
  expressions$final_expression <- c(expressions$N,
                                    expressions$state,
                                    expressions$TC,
                                    expressions$rate_constants,
                                    expressions$shared_reg_terms,
                                    expressions$rate_equations,
                                    expressions$reaction_terms,
                                    expressions$solute_diffcoeffs,
                                    expressions$bounds_c,
                                    expressions$bounds_v,
                                    expressions$transport_terms,
                                    expressions$total_c_change,
                                    expressions$return)
  
  # return created lists (some for informational purpose, some for use)
  return(list(species_operational=species_operational, state=state, names_out=names_out, transport_terms=transport_terms, rate_constants=rate_constants, rate_equations=rate_equations, shared_reg_terms=shared_reg_terms, reaction_terms=reaction_terms, total_c_change=total_c_change, returnlist=returnlist, expressions=expressions))
}


## ---- model_function ----------------------------------------------------------------------------
Model <- function(t, state, pars) {
  eval(model_lists$expressions$final_expression)
}


## ---- steady_state_solving ----------------------------------------------------------------------
ss <- steady.1D(y = model_lists$state,
                time = 0.2,
                func = Model, 
                parms = NULL, 
                names = model_lists$names_out, 
                method = "stode",
                pos = TRUE,
                nspec = length(model_lists$names_out) #,rtol = 1e-16,ctol = 1e-16 ,atol = 1e-16
)


## ---- plots -------------------------------------------------------------------------------------
# draw concentration profiles for species for steady state solution
handlers$concentration_profiles_ss <- function(species=NULL, draw_mode = "facet_wrap", ss_data=ss, trans_data=trans){
  # species: vector of species to draw concentration profile
  # draw_mode: either "facet_wrap", "collective"
  
  # create complete concentrations data-frame 
  df.ss_cs <- data.frame(
    depth = rep(grid_collection$grid$x.mid, length(model_lists$species_operational)),
    concentration = c(ss_data$y),
    species = rep(attributes(ss_data$y)$dimnames[[2]], each=parameters$N))
  
  # correct for solid volume fraction/porosity -> get all concentrations in mol/m³ total volume
  for (element in model_lists$species_operational){
    if (element$phase == "solute"){
      df.ss_cs[df.ss_cs$species == element$name, "concentration"] <- df.ss_cs[df.ss_cs$species == element$name, "concentration"] * grid_collection$por.grid$mid
    }
    else if (element$phase == "solid"){
      df.ss_cs[df.ss_cs$species == element$name, "concentration"] <- df.ss_cs[df.ss_cs$species == element$name, "concentration"] * grid_collection$svf.grid$mid
    }
  }
  
  # get requested subset data frame to plot (if species are specified)
  if (is.null(species)){
    df.plot <- df.ss_cs
  } else {
    df.plot <- df.ss_cs[df.ss_cs$species %in% species, ]
  }
  
  # plot
  myplot <- ggplot(data = df.plot) +
    labs(x="concentration (mol/m³)", y = "depth (m)") + 
    scale_y_continuous(trans = "reverse") +
    theme_light() + 
    theme(strip.text.x = element_text(colour = "black"),
          axis.title = element_text(size = 13))
  
  if (draw_mode == "facet_wrap"){
    myplot <- myplot +
      geom_path(mapping = aes(concentration, depth)) +
      scale_color_discrete(guide = "none") +
      facet_wrap(~species, scales = "free_x", ncol = 4)
  }
  else if (draw_mode == "collective"){
    myplot <- myplot +
      geom_path(mapping = aes(concentration, depth, color=species))
  }
  myplot
}
