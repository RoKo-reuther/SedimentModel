
###########################################################################################################
#                                    CONFIGURE BOUNDARY CONDITIONS HERE                                   #
###########################################################################################################

boundary_conditions <- list(
  
  ### define time-independent boundary conditions here
  constant = list(
    
    ## boundary SWI concentrations for solutes [mol m-3]
    SO4_top         = 8,          # SO4  
    Fe_2_top          = 0,          # Fe   
    Mn_2_top          = 0,          # Mn2+ 
    H2S_top         = 0,          # H2S  
    CH4_top         = 0,         # CH4  
    NH4_top         = 0,          # NH4  
    NO3_top         = 1,          # NO3  
    PO4_top         = 0,          # PO4  
    DIC_top         = 0,          # DIC
    N2_top          = 0,          # N2
    # Cl_top        =  0          # Cl
    
    
    ## boundary SWI fluxes concentrations for solids [mol m-2 y-1]
    F_OrgCA       = 3,             # Org; Highly reactive: Check Multy G model approach (Westrich and Berner, 1984)
    F_OrgCB       = 0,             # Org; Less reactive  
    F_MnO2A      = 1,             # MnO2; Highly reactive   
    F_FeP        = 0,             # FeP 
    F_VivP       = 0,             # Fe3(PO4)2 flux at SWI
    F_FeS        = 0,             # FeS 
    F_FeS2       = 0,             # FeS2
    F_S0         = 0,           # S0 
    F_FeCO3      = 0,           # FeCO3 
    F_MnCO3      = 0,           # MnCO3
    F_OrgP       = 0,             # OrgP #to verify if pool had correct steady state behaviour: later incorporated in po4
    F_OrgN       = 0             # OrgN #idem:but incorporated in nh4
  ),
  
  

  ### define time dependent boundary conditions here
  varying = list(
    
    ## boundary SWI concentrations for solutes [mol m-3]
    # O2
    O2_top = function(t){
      # set "standard value"; at least used for steady state solving
      C_top <-1*0.25
      # vary concentration with time
      if (t >= 5/12 && t < 9/12 ) {
        C_top     <- 0   # anoxic conditions summer months; june july august
      }
      return(C_top)
    },
    
    
    ## boundary SWI fluxes concentrations for solids [mol m-2 y-1]
    # FeOH3A
    F_FeOH3A = function(t){
      # set "standard value"; at least used for steady state solving
      flux <- 2
      # vary flux with time
      if (t >= 1/12 && t < 3/12 ) {
        flux <- 3.8 # 100 grams Fe to mol = 1.79067 mol (additional); february
      }
      return(flux)
    }
  )
)
