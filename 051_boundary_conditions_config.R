
###########################################################################################################
#                                    CONFIGURE BOUNDARY CONDITIONS HERE                                   #
###########################################################################################################

#*************************************
# 1) list of boundary conditions
#*************************************

boundary_conditions <- list(
  
  ### define time-independent boundary conditions here
  constant = list(
    
    ## boundary SWI concentrations for solutes [mol m-3]
    SO4_top         = 0.6,        # SO4  
    Fe_2_top        = 0,          # Fe   
    Mn_2_top        = 0.002,      # Mn2+ 
    H2S_top         = 0,          # H2S  
    CH4_top         = 0,          # CH4 
    PO4_top         = 0,          # PO4  
    DIC_top         = 2.3,        # DIC
    N2_top          = 0,          # N2
    
    
    ## boundary SWI fluxes concentrations for solids [mol m-2 y-1]
    F_OrgCA       = 5,          # Org; Highly reactive: Check Multy G model approach (Westrich and Berner, 1984); 2.5-13
    F_OrgCB       = 0,          # Org; less reactive
    F_OrgCC       = 0,          # Org; non reactive
    F_MnO2A       = 1,          # MnO2; Highly reactive
    F_VivP        = 0,          # Fe3(PO4)2 flux at SWI
    F_FeS         = 0,          # FeS 
    F_FeS2        = 0,          # FeS2
    F_S0          = 0,          # S0 
    F_FeCO3       = 0,          # FeCO3 
    F_MnCO3       = 0           # MnCO3
  ),
  
  

  ### define time dependent boundary conditions here
  varying = list(
    
    ## boundary SWI concentrations for solutes [mol m-3]
    # O2
      # O2_top-function is defined in the section below
      # O2_top = function(t){
      #   # set "standard value"; at least used for steady state solving
      #   C_top <- 1*0.25
      #   # vary concentration with time
      #   if (t%%1 > 5/12 && t%%1 < 8/12 ) {
      #     C_top     <- 0   # anoxic conditions summer months; june july august; every year
      #   }
      #   return(C_top)
      # },
    
    # NH4
      # NO3_top-function is defined in the section below
    
    # NO3
      # NO3_top-function is defined in the section below
    
    
    ## boundary SWI fluxes concentrations for solids [mol m-2 y-1]
    # FeOH3A
    F_FeOH3A = Vectorize(function(t){
      # set "standard value"; at least used for steady state solving
      flux <- 1
      # vary flux with time: add 100 grams/m² Fe in february every year
      # 100g Fe to mol = 1.79067 mol
      # added mass [mol/m²] = flux [mol/(m²*yr)] * timespan [yr] -> flux [mol/(m²*yr)] = 1.8 [mol/m²] / (1/12) [yr]
      if (round(t%%1, digits=15) >= round(1/12, digits=15) && round(t%%1, digits=15) < round(2/12, digits=15)) {
        flux <- flux + 21.6
        #flux <- flux + Fe_addition(t%%1)  # int: 1.774695
      }
      return(flux)
    })
  )
)

#*************************************
# 2) build data series based functions
#*************************************
series_based_functions <- function(){
 
  ## NO3
  # load in data
  temp.data <- read.delim2("./imports/NO3.csv")
  # "extrapolate" loaded partial one-year time series to get complete year cycle in spline
  temp.time <- c(temp.data[["time"]]-1, temp.data[["time"]], temp.data[["time"]]+1)
  temp.value <- rep(temp.data[["NO3"]], 3)
  # evaluate the spline
  temp.spline <- smooth.spline(temp.time, temp.value, spar = 0.5)
  # set all negative values to zero
  temp.spline$y[temp.spline$y < 0] = 0
  # store approximated function in list
  boundary_conditions$varying[["NO3_top"]] <<- approxfun(temp.spline)
  # # check spline
  # plot(temp.value ~ temp.time)
  # lines(temp.spline, col="blue")
  
  ## NH4
  # load in data
  temp.data <- read.delim2("./imports/NH4.csv")
  # "extrapolate" loaded partial one-year time series to get complete year cycle in spline
  temp.time <- c(temp.data[["time"]]-1, temp.data[["time"]], temp.data[["time"]]+1)
  temp.value <- rep(temp.data[["NH4"]], 3)
  # evaluate the spline
  temp.spline <- smooth.spline(temp.time, temp.value, spar = 0.5)
  # set all negative values to zero
  temp.spline$y[temp.spline$y < 0] = 0
  # store approximated function in list
  boundary_conditions$varying[["NH4_top"]] <<- approxfun(temp.spline)
  # # check spline
  # plot(temp.value ~ temp.time)
  # lines(temp.spline, col="blue")
  
  ## O2
  # load in data
  temp.data <- read.delim2("./imports/O2.csv")
  # "extrapolate" loaded partial one-year time series to get complete year cycle in spline
  temp.time <- c(temp.data[["time"]]-1, temp.data[["time"]], temp.data[["time"]]+1)
  temp.value <- rep(temp.data[["O2"]], 3)
  # evaluate the spline
  temp.spline <- smooth.spline(temp.time, temp.value, spar = 0.4)
  # set all negative values to zero
  temp.spline$y[temp.spline$y < 4e-4] = 0
  # store approximated function in list
  boundary_conditions$varying[["O2_top"]] <<- approxfun(temp.spline)
  # # check spline
  # plot(temp.value ~ temp.time)
  # lines(temp.spline, col="blue")
}
series_based_functions()
rm(series_based_functions)