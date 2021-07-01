#=============================================================================
# Transient Scenario
#=============================================================================

########################################## Transient fluxes ######################################
transient <- function(t) {
  #concentrations 
  O2_Trans     <- 1 #multiplication term
  O2_Add       <- 0 #addition term
  SO4_Trans    <- 1 
  SO4_Add      <- 0
  Fe_Trans     <- 1 
  Fe_Add       <- 0
  Mn_Trans     <- 1 
  Mn_Add       <- 0
  H2S_Trans    <- 1 
  H2S_Add      <- 0
  CH4_Trans    <- 1 
  CH4_Add      <- 0
  NH4_Trans    <- 1 
  NH4_Add      <- 0
  NO3_Trans    <- 1 
  NO3_Add      <- 0
  PO4_Trans    <- 1 
  PO4_Add      <- 0
  DIC_Trans    <- 1 
  DIC_Add      <- 0
  N2_Trans     <- 1 
  N2_Add       <- 0 
  
  #fluxes
  OrgA_Trans     <- 1 
  OrgA_Add       <- 0 
  OrgB_Trans     <- 1 
  OrgB_Add       <- 0 
  MnO2A_Trans    <- 1 
  MnO2A_Add      <- 0 
  FeOH3A_Trans   <- 1 
  FeOH3A_Add     <- 0 
  FeP_Trans      <- 1 
  FeP_Add        <- 0 
  VivP_Trans     <- 1 
  VivP_Add       <- 0 
  FeS_Trans      <- 1 
  FeS_Add        <- 0 
  FeS2_Trans     <- 1 
  FeS2_Add       <- 0 
  S0_Trans       <- 1 
  S0_Add         <- 0 
  FeCO3_Trans    <- 1 
  FeCO3_Add      <- 0
  MnCO3_Trans    <- 1 
  MnCO3_Add      <- 0

####################################year 0
  if (t > 1/12 && t < 3/12 ) {
      FeOH3A_Add     <- 1.8 #100 grams Fe to mol = 1.79067 mol
  }
  if (t > 5/12 && t < 9/12 ) {
    O2_Trans     <- 0   #anoxic conditions summer months; june july august
  }
#################################### year 1
  if (t > 1+1/12 && t < 1+3/12 ) {
    FeOH3A_Add     <- 1.8
  }
  if (t > 1+5/12 && t < 1+9/12 ) {
      O2_Trans     <- 0   
  }
####################################  
  if (t > 2+1/12 && t < 2+3/12 ) {
    FeOH3A_Add     <- 1.8
  }
  if (t > 2+5/12 && t < 2+9/12 ) {
    O2_Trans     <- 0   
  }
  #################################### 
  if (t > 3+1/12 && t < 3+3/12 ) {
    FeOH3A_Add     <- 1.8
  }
  if (t > 3+5/12 && t < 3+9/12 ) {
    O2_Trans     <- 0   
  }
  #################################### 
  if (t > 4+1/12 && t < 4+3/12 ) {
    FeOH3A_Add     <- 1.8
  }
  if (t > 4+5/12 && t < 4+9/12 ) {
    O2_Trans     <- 0   
  }
  #################################### 
  if (t > 5+1/12 && t < 5+3/12 ) {
    FeOH3A_Add     <- 1.8
  }
  if (t > 5+5/12 && t < 5+9/12 ) {
    O2_Trans     <- 0   
  }
  #################################### 
  if (t > 6+1/12 && t < 6+3/12 ) {
    FeOH3A_Add     <- 1.8
  }
  if (t > 6+5/12 && t < 6+9/12 ) {
    O2_Trans     <- 0   
  }
  #################################### 
  
  if (t > 7+1/12 && t < 7+3/12 ) {
    FeOH3A_Add     <- 1.8
  }
  if (t > 7+5/12 && t < 7+9/12 ) {
    O2_Trans     <- 0   
  }
  #################################### 
  if (t > 8+1/12 && t < 8+3/12 ) {
    FeOH3A_Add     <- 1.8
  }
  if (t > 8+5/12 && t < 8+9/12 ) {
    O2_Trans     <- 0   
  }
  #################################### 
  if (t > 9+1/12 && t < 9+3/12 ) {
    FeOH3A_Add     <- 1.8
  }
  if (t > 9+5/12 && t < 9+9/12 ) {
    O2_Trans     <- 0   
  }
  #################################### 
  if (t > 10+1/12 && t < 10+3/12 ) {
    FeOH3A_Add     <- 1.8
  }
  if (t > 10+5/12 && t < 10+9/12 ) {
    O2_Trans     <- 0   
  }
  #################################### 
  if (t > 11+1/12 && t < 11+3/12 ) {
    FeOH3A_Add     <- 1.8
  }
  if (t > 11+5/12 && t < 11+9/12 ) {
    O2_Trans     <- 0   
  }
  #################################### 
  if (t > 12+1/12 && t < 12+3/12 ) {
    FeOH3A_Add     <- 1.8
  }
  if (t > 12+5/12 && t < 12+9/12 ) {
    O2_Trans     <- 0   
  }
  #################################### 
  if (t > 13+1/12 && t < 13+3/12 ) {
    FeOH3A_Add     <- 1.8
  }
  if (t > 13+5/12 && t < 13+9/12 ) {
    O2_Trans     <- 0   
  }
  #################################### 
  if (t > 14+1/12 && t < 14+3/12 ) {
    FeOH3A_Add     <- 1.8
  }
  if (t > 14+5/12 && t < 14+9/12 ) {
    O2_Trans     <- 0   
  }
  #################################### 
  if (t > 15+1/12 && t < 15+3/12 ) {
    FeOH3A_Add     <- 1.8
  }
  if (t > 15+5/12 && t < 15+9/12 ) {
    O2_Trans     <- 0   
  }
  #################################### 
  if (t > 16+1/12 && t < 16+3/12 ) {
    FeOH3A_Add     <- 1.8
  }
  if (t > 16+5/12 && t < 16+9/12 ) {
    O2_Trans     <- 0   
  }
  #################################### 
  if (t > 17+1/12 && t < 17+3/12 ) {
    FeOH3A_Add     <- 1.8
  }
  if (t > 17+5/12 && t < 17+9/12 ) {
    O2_Trans     <- 0   
  }
  #################################### 
  if (t > 18+1/12 && t < 18+3/12 ) {
    FeOH3A_Add     <- 1.8
  }
  if (t > 18+5/12 && t < 18+9/12 ) {
    O2_Trans     <- 0   
  }
  #################################### 
  if (t > 19+1/12 && t < 19+3/12 ) {
    FeOH3A_Add     <- 1.8
  }
  if (t > 19+5/12 && t < 19+9/12 ) {
    O2_Trans     <- 0   
  }
  #################################### 
  
  return(c( O2_Trans,
            O2_Add,       
            SO4_Trans,     
            SO4_Add,      
            Fe_Trans,      
            Fe_Add,       
            Mn_Trans,      
            Mn_Add,       
            H2S_Trans,     
            H2S_Add,      
            CH4_Trans,     
            CH4_Add,      
            NH4_Trans,     
            NH4_Add,      
            NO3_Trans,     
            NO3_Add,      
            PO4_Trans,     
            PO4_Add,      
            DIC_Trans,     
            DIC_Add,      
            N2_Trans,      
            N2_Add,       
            OrgA_Trans,      
            OrgA_Add,       
            OrgB_Trans,      
            OrgB_Add,       
            MnO2A_Trans,      
            MnO2A_Add,       
            FeOH3A_Trans,      
            FeOH3A_Add,       
            FeP_Trans,      
            FeP_Add,       
            VivP_Trans,      
            VivP_Add,       
            FeS_Trans,      
            FeS_Add,       
            FeS2_Trans,      
            FeS2_Add,       
            S0_Trans,      
            S0_Add,       
            FeCO3_Trans,      
            FeCO3_Add,       
            MnCO3_Trans,      
            MnCO3_Add))
}
