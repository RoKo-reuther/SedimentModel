
###########################################################################################################
#                                    CONFIGURE BOUNDARY CONDITIONS HERE                                   #
###########################################################################################################


# configure "standard boundary-conditions":

boundary_conditions <- list(
  #boundary SWI concentrations for solutes [mol m-3]
  O2_top          = 1*0.25,     # O2  
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
  
  #boundary SWI fluxes concentrations for solids [mol m-2 y-1]
  F_OrgCA       = 3,             # Org; Highly reactive: Check Multy G model approach (Westrich and Berner, 1984)
  F_OrgCB       = 0,             # Org; Less reactive  
  F_MnO2A      = 1,             # MnO2; Highly reactive 
  F_FeOH3A     = 2,            # FeOX; Labile   
  F_FeP        = 0,             # FeP 
  F_VivP       = 0,             # Fe3(PO4)2 flux at SWI
  F_FeS        = 0,             # FeS 
  F_FeS2       = 0,             # FeS2
  F_S0         = 0,           # S0 
  F_FeCO3      = 0,           # FeCO3 
  F_MnCO3      = 0,           # MnCO3
  F_OrgP       = 0,             # OrgP #to verify if pool had correct steady state behaviour: later incorporated in po4
  F_OrgN       = 0             # OrgN #idem:but incorporated in nh4
)




# configure transient boundary behaviour:
  # boundary conditions are calculated in the Model-function as follows:
    # solutes: X_top = (X_top + X_Add) * X_Trans
    # solids: F_X = (F_X + X_Add) * X_Trans

#################################### year 0
if (t >= 1/12 && t < 3/12 ) {
  FeOH3A_Add     <- 1.8 #100 grams Fe to mol = 1.79067 mol; february
}
if (t >= 5/12 && t < 9/12 ) {
  O2_Trans     <- 0   #anoxic conditions summer months; june july august
}
#################################### year 1
if (t >= 1+1/12 && t < 1+3/12 ) {
  FeOH3A_Add     <- 1.8
}
if (t >= 1+5/12 && t < 1+9/12 ) {
  O2_Trans     <- 0   
}
####################################  
if (t >= 2+1/12 && t < 2+3/12 ) {
  FeOH3A_Add     <- 1.8
}
if (t >= 2+5/12 && t < 2+9/12 ) {
  O2_Trans     <- 0   
}
#################################### 
if (t >= 3+1/12 && t < 3+3/12 ) {
  FeOH3A_Add     <- 1.8
}
if (t >= 3+5/12 && t < 3+9/12 ) {
  O2_Trans     <- 0   
}
#################################### 
if (t >= 4+1/12 && t < 4+3/12 ) {
  FeOH3A_Add     <- 1.8
}
if (t >= 4+5/12 && t < 4+9/12 ) {
  O2_Trans     <- 0   
}
#################################### 
if (t >= 5+1/12 && t < 5+3/12 ) {
  FeOH3A_Add     <- 1.8
}
if (t >= 5+5/12 && t < 5+9/12 ) {
  O2_Trans     <- 0   
}
#################################### 
if (t >= 6+1/12 && t < 6+3/12 ) {
  FeOH3A_Add     <- 1.8
}
if (t >= 6+5/12 && t < 6+9/12 ) {
  O2_Trans     <- 0   
}
#################################### 

if (t >= 7+1/12 && t < 7+3/12 ) {
  FeOH3A_Add     <- 1.8
}
if (t >= 7+5/12 && t < 7+9/12 ) {
  O2_Trans     <- 0   
}
#################################### 
if (t >= 8+1/12 && t < 8+3/12 ) {
  FeOH3A_Add     <- 1.8
}
if (t >= 8+5/12 && t < 8+9/12 ) {
  O2_Trans     <- 0   
}
#################################### 
if (t >= 9+1/12 && t < 9+3/12 ) {
  FeOH3A_Add     <- 1.8
}
if (t >= 9+5/12 && t < 9+9/12 ) {
  O2_Trans     <- 0   
}
#################################### 
if (t >= 10+1/12 && t < 10+3/12 ) {
  FeOH3A_Add     <- 1.8
}
if (t >= 10+5/12 && t < 10+9/12 ) {
  O2_Trans     <- 0   
}
#################################### 
if (t >= 11+1/12 && t < 11+3/12 ) {
  FeOH3A_Add     <- 1.8
}
if (t >= 11+5/12 && t < 11+9/12 ) {
  O2_Trans     <- 0   
}
#################################### 
if (t >= 12+1/12 && t < 12+3/12 ) {
  FeOH3A_Add     <- 1.8
}
if (t >= 12+5/12 && t < 12+9/12 ) {
  O2_Trans     <- 0   
}
#################################### 
if (t >= 13+1/12 && t < 13+3/12 ) {
  FeOH3A_Add     <- 1.8
}
if (t >= 13+5/12 && t < 13+9/12 ) {
  O2_Trans     <- 0   
}
#################################### 
if (t >= 14+1/12 && t < 14+3/12 ) {
  FeOH3A_Add     <- 1.8
}
if (t >= 14+5/12 && t < 14+9/12 ) {
  O2_Trans     <- 0   
}
#################################### 
if (t >= 15+1/12 && t < 15+3/12 ) {
  FeOH3A_Add     <- 1.8
}
if (t >= 15+5/12 && t < 15+9/12 ) {
  O2_Trans     <- 0   
}
#################################### 
if (t >= 16+1/12 && t < 16+3/12 ) {
  FeOH3A_Add     <- 1.8
}
if (t >= 16+5/12 && t < 16+9/12 ) {
  O2_Trans     <- 0   
}
#################################### 
if (t >= 17+1/12 && t < 17+3/12 ) {
  FeOH3A_Add     <- 1.8
}
if (t >= 17+5/12 && t < 17+9/12 ) {
  O2_Trans     <- 0   
}
#################################### 
if (t >= 18+1/12 && t < 18+3/12 ) {
  FeOH3A_Add     <- 1.8
}
if (t >= 18+5/12 && t < 18+9/12 ) {
  O2_Trans     <- 0   
}
#################################### 
if (t >= 19+1/12 && t < 19+3/12 ) {
  FeOH3A_Add     <- 1.8
}
if (t >= 19+5/12 && t < 19+9/12 ) {
  O2_Trans     <- 0   
}
####################################