
###########################################################################################################
#                                             MODEL FORMULATION                                           #
###########################################################################################################

Model <- function(t, state, pars) {
  
  ## Initialisation of state variables
  for (i in seq_along(species_operational)){
    assign(names(species_operational[i]), state[((i-1)*N+1):(i*N)])
  }
  
  
  ## Define boundary conditions
  
    # define "transient terms"
    Change_factor <- transient(t)
    
    O2_Trans  <- Change_factor[1]
    O2_Add    <- Change_factor[2]
    SO4_Trans <- Change_factor[3]
    SO4_Add   <- Change_factor[4]
    Fe_Trans  <- Change_factor[5]
    Fe_Add    <- Change_factor[6]
    Mn_Trans  <- Change_factor[7]
    Mn_Add    <- Change_factor[8]
    H2S_Trans <- Change_factor[9]
    H2S_Add   <- Change_factor[10]
    CH4_Trans <- Change_factor[11]
    CH4_Add   <- Change_factor[12]
    NH4_Trans <- Change_factor[13]
    NH4_Add   <- Change_factor[14]
    NO3_Trans <- Change_factor[15]
    NO3_Add   <- Change_factor[16]
    PO4_Trans <- Change_factor[17]
    PO4_Add   <- Change_factor[18]
    DIC_Trans <- Change_factor[19]
    DIC_Add   <- Change_factor[20]
    N2_Trans  <- Change_factor[21]
    N2_Add    <- Change_factor[22]
    OrgA_Trans<- Change_factor[23]
    OrgA_Add  <- Change_factor[24]
    OrgB_Trans<- Change_factor[25]
    OrgB_Add  <- Change_factor[26]
    MnO2A_Trans <- Change_factor[27]
    MnO2A_Add   <- Change_factor[28]
    FeOH3A_Trans<- Change_factor[29]
    FeOH3A_Add  <- Change_factor[30]
    FeP_Trans   <- Change_factor[31]
    FeP_Add     <- Change_factor[32]
    VivP_Trans  <- Change_factor[33]
    VivP_Add    <- Change_factor[34]
    FeS_Trans   <- Change_factor[35]
    FeS_Add     <- Change_factor[36]
    FeS2_Trans  <- Change_factor[37]
    FeS2_Add    <- Change_factor[38]
    S0_Trans    <- Change_factor[39]
    S0_Add      <- Change_factor[40]
    FeCO3_Trans <- Change_factor[41]
    FeCO3_Add   <- Change_factor[42]
    MnCO3_Trans <- Change_factor[43]
    MnCO3_Add   <- Change_factor[44]
  
    # define boundary conditions (concentrations for solutes and fluxes for solids)
    O2_top       <- (O2_top + O2_Add)*O2_Trans         
    SO4_top      <- (SO4_top + SO4_Add)*SO4_Trans  
    Fe_top       <- (Fe_top + Fe_Add)*Fe_Trans    
    Mn_top       <- (Mn_top + Mn_Add)*Mn_Trans  
    H2S_top      <- (H2S_top + H2S_Add)*H2S_Trans   
    CH4_top      <- (CH4_top + CH4_Add)*CH4_Trans   
    NH4_top      <- (NH4_top + NH4_Add)*NH4_Trans   
    NO3_top      <- (NO3_top + NO3_Add)*NO3_Trans   
    PO4_top      <- (PO4_top + PO4_Add)*PO4_Trans   
    HCO3_top      <- (DIC_top + DIC_Add)*DIC_Trans 
    N2_top       <- (N2_top + N2_Add)*N2_Trans 
    F_OrgCA       <- (F_OrgA + OrgA_Add)*OrgA_Trans
    F_OrgCB       <- (F_OrgB + OrgB_Add)*OrgB_Trans  
    F_MnO2A      <- (F_MnO2A + MnO2A_Add)*MnO2A_Trans 
    F_FeOH3A     <- (F_FeOH3A + FeOH3A_Add)*FeOH3A_Trans
    F_FeP        <- (F_FeP + FeP_Add)*FeP_Trans 
    F_VivP       <- (F_VivP + VivP_Add)*VivP_Trans 
    F_FeS        <- (F_FeS + FeS_Add)*FeS_Trans  
    F_FeS2       <- (F_FeS2 + FeS2_Add)*FeS2_Trans
    F_S0         <- (F_S0 + S0_Add)*S0_Trans  
    F_FeCO3      <- (F_FeCO3 + FeCO3_Add)*FeCO3_Trans 
    F_MnCO3      <- (F_MnCO3 + MnCO3_Add)*MnCO3_Trans
    
  
  ## Define the transport terms
  for (i in seq_along(transport_terms)){
    assign(names(transport_terms[i]), eval(parse(text=transport_terms[[i]]))) 
  }
  
  
  ## Define the reaction terms
    # assign reaction rate constants
    for (i in seq_along(rate_constants)){
      assign(names(rate_constants[i]), rate_constants[[i]])
      }
    
    # assign shared regulation terms
    for (i in seq_along(shared_reg_terms)){
      assign(names(shared_reg_terms[i]), eval(parse(text=shared_reg_terms[[i]])))
    }
    
    # assign rate equations
    for (i in seq_along(rate_equations)){
      assign(names(rate_equations[i]), eval(parse(text=rate_equations[[i]]))) 
    }
  
    # assign reaction terms
    for (i in seq_along(reaction_terms)){
      assign(names(reaction_terms[i]), eval(parse(text=reaction_terms[[i]]))) 
    }
  
    
  ## Define "total concentration change terms"
  for (i in seq_along(total_c_change)){
    assign(names(total_c_change[i]), eval(parse(text=total_c_change[[i]]))) 
  }
  
  
  ## return statement
  return(eval(parse(text = returnlist)))
}