require(deSolve)
require(marelac)
require(ReacTran)
require(rootSolve)
require(FME)

### load sediment data
data_list <- list(
  #FeOH3A_d = read.delim2("~/Dokumente/SedimentModel/imports/FeOH3A_d.csv"),
  #FeS2_d = read.delim2("~/Dokumente/SedimentModel/imports/FeS2_d.csv"),
  #Fe_2_d = read.delim2("~/Dokumente/SedimentModel/imports/Fe_2_d.csv"),
  #NO3_d = read.delim2("~/Dokumente/SedimentModel/imports/NO3_d.csv"),
  #H2S_d = read.delim2("~/Dokumente/SedimentModel/imports/H2S_d.csv"),
  #SO4_d = read.delim2("~/Dokumente/SedimentModel/imports/SO4_d.csv"),
  O2_d = read.delim2("~/Dokumente/SedimentModel/imports/O2_d.csv"),
  OM_d = read.delim2("~/Dokumente/SedimentModel/imports/OM_d.csv") # w%OM of sediment weight
)

  # create merged data frame
  merge_data <- function(){
    merged_D <- data_list[[1]]
    merge_data_sub <- function(i){
      merge(merged_D, data_list[[i]], all = TRUE, sort = TRUE)
    }
    for (i in seq_along(data_list)[-1]){
      merged_D <- merge_data_sub(i)
    }
    return(merged_D)
  }
  DATA <- merge_data()


### collection of all parameters
parms_coll <- list(
  # parms: initial values
  default = c(Db         = 4e-4,
            Db_depth     = 0.1,
            
            k_alpha      = 1.62,
            k_beta       = 0.0086,
            #K_mO2        = 1e-3,
            #K_mNO3       = 4e-3,
            #K_mMnO2      = 0.2*grid_collection$sed_dens.grid$mid,
            #K_mFeOH3     = 0.65*grid_collection$sed_dens.grid$mid,
            #K_mSO4       = 1.6e-3,
            #k1           = 5e3, # Nitrifikation
            #k2           = 1.4e2, # FeOH3 formation
            #k5           = 1.6e2, # H2S oxidation
            #k6           = 1e8, # aerobic methane oxidation
            #k7           = 8, # FeOxA reduction coupled to sulphide oxidation
            #k9           = 0.1482, # FeS formation
            #k13          = 10, # SO4 reduction coupled to AOM
            #k23          = 0.265, # MnCO3 precipitation
            #k24          = 1, # Mn oxidation
            #k25          = 23.652, # MnO2A reduction coupled to Fe oxidation
            #k27          = 4e4, # MnO2A reduction coupled to S oxidation
            #k30          = 1.7e-3, # MnO2A reduction coupled to AOM
            #k18          = 1.89e-2, # FeCO3 formation with CO32-
            #k32          = 1.89e-2, # FeCO3 dissolution
            #k20          = 1.15e-1, # vivianite formation
            #k21          = 1.15e-1, # vivianite dissolution
            #k22          = 8e-4, # conversion of vivianite to FeS
            #k3           = 6e1, # FeS dissolution
            #k4           = 5e3, # FeS2 dissolution
            #k10          = 30, # pyrite formation with H2S
            #k11          = 1.6e2, # S0 transformation into SO4 and H2S
            #k12          = 7.26e3, # pyrite formation with S0
            #k19          = 8e-4, # conversion of FeCO3 to FeS
            
            F_OrgC_tot   = 5.479127     # total OM influx: F_OrgCA + F_OrgCB + F_OrgCC
            #F_MnO2A      = 0.4527190,
            #F_FeOH3A     = 0.1640076
  ),
  
  # parms: lower limit
  lower = c(Db           = 4e-4,
            Db_depth     = 0.05,
            
            k_alpha      = 0.05,
            k_beta       = 0.0025,
            #K_mO2        = 1e-3,
            #K_mNO3       = 4e-3,
            #K_mMnO2      = 0.004*grid_collection$sed_dens.grid$mid,
            #K_mFeOH3     = 0.65*grid_collection$sed_dens.grid$mid,
            #K_mSO4       = 1.6e-3,
            #k1           = 5e3,
            #k2           = 1.4e2, # FeOH3 formation
            #k5           = 1.6e2, # H2S oxidation
            #k6           = 1e8, # aerobic methane oxidation
            #k7           = 8, # FeOxA reduction coupled to sulphide oxidation
            #k9           = 1e-4, # FeS formation
            #k13          = 10, # SO4 reduction coupled to AOM
            #k23          = 0.265, # MnCO3 precipitation
            #k24          = 0, # Mn oxidation
            #k25          = 23.652, # MnO2A reduction coupled to Fe oxidation
            #k27          = 20, # MnO2A reduction coupled to S oxidation
            #k30          = 1.7e-3,  # MnO2A reduction coupled to AOM
            #k18          = 2.7e-3, # FeCO3 formation with CO32-
            #k32          = 1.89e-2, # FeCO3 dissolution
            #k20          = 1.15e-1, # vivianite formation
            #k21          = 1.15e-1, # vivianite dissolution
            #k22          = 8e-4, # conversion of vivianite to FeS
            #k3           = 6e1, # FeS dissolution
            #k4           = 5e3, # FeS2 dissolution
            #k10          = 3e-4, # pyrite formation with H2S
            #k11          = 1.6e2, # S0 transformation into SO4 and H2S
            #k12          = 7.26e3, # pyrite formation with S0
            #k19          = 8e-4, # conversion of FeCO3 to FeS
            
            F_OrgC_tot   = 2.5
            #F_MnO2A      = 0.4527190,
            #F_FeOH3A     = 0.1640076
  ),
  
  # parms: upper limit
  upper = c(Db           = 4e-3,
            Db_depth     = 0.2,
            
            k_alpha      = 1.62,
            k_beta       = 0.0086,
            #K_mO2        = 3e-2,
            #K_mNO3       = 8e-2,
            #K_mMnO2      = 0.032*grid_collection$sed_dens.grid$mid,
            #K_mFeOH3     = 1*grid_collection$sed_dens.grid$mid,
            #K_mSO4       = 1.6e-3,
            #k1           = 20e4,
            #k2           = 1.4e2, # FeOH3 formation
            #k5           = 10e2, # H2S oxidation
            #k6           = 1e8, # aerobic methane oxidation
            #k7           = 10e2, # FeOxA reduction coupled to sulphide oxidation
            #k9           = 1.4832e4, # FeS formation
            #k13          = 120, # SO4 reduction coupled to AOM
            #k23          = 0.265, # MnCO3 precipitation
            #k24          = 10e3, # Mn oxidation
            #k25          = 10e4, # MnO2A reduction coupled to Fe oxidation
            #k27          = 1e5, # MnO2A reduction coupled to S oxidation
            #k30          = 1.7e-3, # MnO2A reduction coupled to AOM
            #k18          = 1.89e-2, # FeCO3 formation with CO32-
            #k32          = 1.89e-2, # FeCO3 dissolution
            #k20          = 1.15e-1, # vivianite formation
            #k21          = 1.15e-1, # vivianite dissolution
            #k22          = 8e-4, # conversion of vivianite to FeS
            #k3           = 3.2e3, # FeS dissolution
            #k4           = 5e4, # FeS2 dissolution
            #k10          = 30, # pyrite formation with H2S
            #k11          = 1.6e2, # S0 transformation into SO4 and H2S
            #k12          = 7.26e3, # pyrite formation with S0
            #k19          = 8e-4, # conversion of FeCO3 to FeS
            
            F_OrgC_tot   = 13.33
            #F_MnO2A      = 0.4527190,
            #F_FeOH3A     = 0.1640076
  )
)


### run model for given parameter set
std.fun <- function(pars){

  # the model takes all parameters, functions to evaluate etc. from lists in .GlobalEnv
  # modify this lists according to given parameter set
  try({
    boundary_conditions$constant$F_OrgCA  <<- pars[['F_OrgC_tot']] * 0.50
    boundary_conditions$constant$F_OrgCB  <<- pars[['F_OrgC_tot']] * 0.16
    boundary_conditions$constant$F_OrgCC  <<- pars[['F_OrgC_tot']] * 0.34
    boundary_conditions$constant$F_MnO2A  <<- pars[['F_MnO2A']]
    boundary_conditions$constant$F_FeOH3A <<- pars[['F_FeOH3A']]
    boundary_conditions$constant$F_FeCO3  <<- pars[['F_FeCO3']]
    boundary_conditions$constant$F_MnCO3  <<- pars[['F_MnCO3']]
    
    rate_constants$k_alpha <<- pars[['k_alpha']]
    rate_constants$k_beta <<- pars[['k_beta']]
    rate_constants$K_mO2 <<- pars[['K_mO2']]
    rate_constants$K_mNO3 <<- pars[['K_mNO3']]
    rate_constants$K_mMnO2 <<- pars[['K_mMnO2']]
    rate_constants$K_mFeOH3 <<- pars[['K_mFeOH3']]
    rate_constants$K_mSO4 <<- pars[['K_mSO4']]
    rate_constants$k1 <<- pars[['k1']]
    
    grid_collection$Db.grid <<- setup.prop.1D(func=p.sig, grid = grid_collection$grid, y.0 = pars[['Db']], y.inf = 0,x.L = pars[['Db_depth']], x.att = 0.05)
  }, silent = TRUE)
  
  # run model
    if(exists("ss_temp")){
      state <- as.vector(ss_temp$y)
    } else{
      state <- rep(0, length.out = parameters$N*length(species_operational))
    }
    print(system.time(
      ss_temp <<- steady.1D(y = state,
                           time = 0.35,
                           func = Model, 
                           parms = NULL, 
                           names = names_out, 
                           method = "stodes",
                           #verbose=TRUE,
                           pos = TRUE,
                           nspec = length(names_out)
                           ,rtol = 9e-13,ctol = 9e-13 ,atol = 9e-13
      )))
    
    # prepare data to return
    # calculate %OM
    # ASSUMPTIONS:
      # LOI ~ %OM (referred to solid fraction)
      # weight of one mole OM in this model: 33.64 g/mol
    spec_selection <- attributes(ss_temp$y)$dimnames[[2]] %in% c("OrgCA", "OrgCB", "OrgCC")
    total_OrgC     <- apply(ss_temp$y[, spec_selection], c(1), sum) # mol/m³_sf
    total_OrgC     <- total_OrgC * 33.64151 # g/m³_sf
    perc_OM        <- (total_OrgC / (grid_collection$sed_dens.grid$mid*1e6))*100 # %OM
    # select other species to fit by name
    spec_selection <- attributes(ss_temp$y)$dimnames[[2]] %in% attributes(DATA)$names
    other_species  <- matrix(ss_temp$y[, spec_selection], dimnames=list(NULL , attributes(ss_temp$y)$dimnames[[2]][spec_selection]))
    # store data of interest in data frame
    data_raw <- data.frame(depth = grid_collection$grid$x.mid, OM = perc_OM, other_species)
    # approximate values for depths occurring in (measured) DATA
    approx_sub <- function(i){
      approx_values      <- approx(data_raw[['depth']], data_raw[[i]], xout = DATA[['depth']])
      temp_result        <- data.frame(approx_values$y)
      names(temp_result) <- attributes(data_raw)$names[i]
      return(temp_result)
    }
    model_data <- DATA['depth']
    for (i in seq_along(data_raw)[-1]){
      model_data <- data.frame(model_data, approx_sub(i))
    }
  return(model_data)
}


### cost function
modCost.fun <- function(fitpar, OBS){
  p                <- parms_coll$default
  p[names(fitpar)] <- fitpar
  
  return(modCost(model = std.fun(p),
                 obs = OBS,
                 x = "depth",
                 weight = "mean",
                 scaleVar = TRUE))
}


### fitting
p_selection <- c("F_OrgC_tot", "Db", "Db_depth", "k_alpha", "k_beta")
pIni <- parms_coll$default[p_selection]
pMin <- parms_coll$lower[p_selection]
pMax <- parms_coll$upper[p_selection]

pIni <- PFit$par # iterative fitting using different methods

## simple fitting
PFit <- modFit(f=modCost.fun, OBS=DATA, p=pIni, lower=pMin, upper=pMax, method = "Marq")
SFit <- summary(PFit)
SFit

## advanced fitting
# Markov Chain
Covar   <- SFit$cov.scaled * 2.4^2/3
s2prior <- SFit$modVariance
N.iter  <- 10000

MCMC <- modMCMC(f=modCost.fun, p=PFit$par, lower=pMin, upper=pMax, OBS=DATA,
                var0=s2prior, jump=Covar, wvar0=1, updatecov=100, niter=N.iter)
summary(MCMC)
plot(MCMC)
pairs(MCMC)

# sensitivity range
sR <- sensRange(parms=parms_coll$default, parInput=MCMC$par, f=std.fun, num=100)
plot(summary(sR), xyswap=TRUE, obs=DATA, obspar=list(pch=16, col="red"))


### plotting depth profiles
BestFit <- std.fun(PFit$par)
FME::obsplot(BestFit, xyswap=TRUE, ylim=c(0.2, 0), type=c("l", "p"), lwd=2, las=1, ylab="m",
     obs=DATA)
