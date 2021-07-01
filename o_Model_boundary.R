#=============================================================================
# Boundary conditions
#=============================================================================

#boundary SWI concentrations for solutes [mol m-3]
O2_top          <- 1*0.25     # O2  
SO4_top         <- 8          # SO4  
Fe_top          <- 0         # Fe   
Mn_top          <- 0          # Mn2+ 
H2S_top         <- 0          # H2S  
CH4_top         <- 0          # CH4  
NH4_top         <- 0          # NH4  
NO3_top         <- 1          # NO3  
PO4_top         <- 0          # PO4  
DIC_top         <- 0          # DIC
N2_top          <- 0          # N2
# Cl_top          <- 0          # Cl

#boundary SWI fluxes concentrations for solids [mol m-2 y-1]
F_OrgA       <- 3             # Org; Highly reactive: Check Multy G model approach (Westrich and Berner, 1984)
F_OrgB       <- 0             # Org; Less reactive  
F_MnO2A      <- 1             # MnO2; Highly reactive 
F_FeOH3A     <- 2             # FeOX; Labile   
F_FeP        <- 0             # FeP 
F_VivP       <- 0             # Fe3(PO4)2 flux at SWI
F_FeS        <- 0             # FeS 
F_FeS2       <- 0             # FeS2
F_S0         <- 0           # S0 
F_FeCO3      <- 0           # FeCO3 
F_MnCO3      <- 0           # MnCO3
F_OrgP       <- 0             # OrgP #to verify if pool had correct steady state behaviour: later incorporated in po4
F_OrgN       <- 0             # OrgN #idem:but incorporated in nh4

# F_OrgC       <- 0           # OrgC; Unreactive: Refractory
# F_FeOH3B     <- 0           # FeOX; Less reactive
# F_FeOH3B_2   <- 0
# F_FeOH3B_pulse <- 0
# F_FeOH3C     <- 0           # FeOX; Refractory: Doesn't react with H2S, nor with CH4 or OM
# F_MnO2B      <- 0           # MnO2; Less reactive  
# F_CaP        <- 0           # Authigenic Ca-P 
# F_DetrP      <- 0           # Detrital P
