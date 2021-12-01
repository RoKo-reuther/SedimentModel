
###########################################################################################################
#                                    CONFIGURE BOUNDARY CONDITIONS HERE                                   #
###########################################################################################################

# up: upstream boundary; down: downstream boundary
# a) no argument -> zero gradient
# b) concentrations for solutes [mol m-3] as value, "annual-cycle" or custom function
# c) fluxes for solids [mol m-2 y-1] as value, "annual-cycle" or custom function

boundary_conditions <- list(
  
  SO4    = list(up = 0.6, down = 1),
  Fe_2   = list(up = 0, down = function(t){1+1}),
  Mn_2   = list(down = 0.002),
  H2S    = list(up = 0),
  CH4    = list(up = 0),
  PO4    = list(up = 0),
  DIC    = list(up = 2.3),
  N2     = list(up = 0),
  O2     = list(up = handlers$annual_cycle("./imports/O2.csv", 0.4)),
  NO3    = list(up = handlers$annual_cycle("./imports/NO3.csv")),
  NH4    = list(up = handlers$annual_cycle("./imports/NH4.csv")),
  
  OrgCA  = list(up = 2.5),  # Org; Highly reactive: Check Multy G model approach (Westrich and Berner, 1984); 2.5-13
  OrgCB  = list(up = 1),    # Org; less reactive
  OrgCC  = list(up = 2),    # Org; non reactive
  MnO2A  = list(up = 1),    # MnO2; Highly reactive
  VivP   = list(up = 0),    # Fe3(PO4)2 flux at SWI
  FeS    = list(up = 0),
  FeS2   = list(up = 0),
  S0     = list(up = 0),
  FeCO3  = list(up = 0),
  MnCO3  = list(up = 0),
  FeOH3A = list(up = function(t){
                        t <- t%%1
                        # set "standard value"; at least used for steady state solving
                        flux <- 1
                        # vary flux with time: add 100 grams/m² Fe in february every year
                        # 100g Fe to mol = 1.79067 mol
                        # added mass [mol/m²] = flux [mol/(m²*yr)] * timespan [yr] -> flux [mol/(m²*yr)] = 1.8 [mol/m²] / (1/12) [yr]
                        if (round(t, digits=15) >= round(1/12, digits=15) && round(t, digits=15) < round(2/12, digits=15)) {
                          flux <- flux + 21.6
                          #flux <- flux + Fe_addition(t%%1)  # int: 1.774695
                        }
                        return(flux)
                      })
)
