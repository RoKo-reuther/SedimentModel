#=============================================================================
# Initialization and names transient scenario
#=============================================================================

#variables/parameters that have a different value  over depth
names_trans_depth <- c("OrgCA",
                 "OrgCB",
                 "O2",
                 "NO3",
                 "MnO2A",
                 "FeOH3A",
                 "FeP",
                 "SO4",
                 "CH4",
                 "NH4",
                 "PO4",
                 "Fe",
                 "VivP",
                 "OrgP",
                 "DIC",
                 "Mn",
                 "OrgN",
                 "H2S",
                 "FeS",
                 "FeS2",
                 "S0",
                 "FeCO3",
                 "MnCO3",
                 "N2",
                 "R1a",
                 "R1b",
                 "RNa",
                 "RNb",
                 "RMa",
                 "RMb",
                 "R2a_Ox",
                 "R2b_Ox",
                 "R2a_P",
                 "R2b_P",
                 "R3a",
                 "R3b",
                 "R4a",
                 "R4b",
                 "R19",
                 "R5_P",
                 "R5_Ox",
                 "R6",
                 "R7",
                 "R8",
                 #"R9",
                 "R10_Ox",
                 "R10_P",
                 #"R17_Ox",
                 #"R17_P",
                 "R11",
                 "R20",
                 "R14",
                 "R15",
                 "R12",
                 #"R13_Ox",
                 #"R13_P",
                 #"R18_Ox",
                 #"R18_P",
                 #"R16",
                 #"R24",
                 "R25",
                 "R26",
                 "R27",
                 "R28",
                 "R29_Ox",
                 "R29_P",
                 #"R32_Ox",
                 #"R32_P",
                 "R30",
                 #"R31",
                 #"R33",
                 "R34",
                 #"R35",
                 "D_PO4",
                 "omega_FeCO3",
                 "precip_rate_FeCO3",
                 "diss_rate_FeCO3",
                 "omega_viv",
                 "precip_rate_viv",
                 "diss_rate_viv",
                 "ROrgCA",
                 "ROrgCB",
                 "RO2",
                 "RNO3",
                 "RMnO2A",
                 "RFeOH3A",
                 "RFeP",
                 "RSO4",
                 "RCH4",
                 "RNH4",
                 "RPO4",
                 "RFe",
                 "RVivP",
                 "ROrgP",
                 "RDIC",
                 "RMn",
                 "ROrgN",
                 "RH2S",
                 "RFeS",
                 "RFeS2",
                 "RS0",
                 "RFeCO3",
                 "RMnCO3",
                 "RN2",
                 "IAP_FeCO3",
                 "IAP_viv")
                 
#variables/parameters that have a boundary (single) value
names_trans_boundary <- c(        
                 "Ksp_FeCO3",
                 "Ksp_viv",
                 "FU_OrgCA",
                 "FD_OrgCA",
                 "FU_OrgCB",
                 "FD_OrgCB",
                 "FU_O2",
                 "FD_O2",
                 "FU_NO3",
                 "FD_NO3",
                 "FU_MnO2A",
                 "FD_MnO2A",
                 "FU_FeOH3A",
                 "FD_FeOH3A",
                 "FU_FeP",
                 "FD_FeP",
                 "FU_SO4",
                 "FD_SO4",
                 "FU_CH4",
                 "FD_CH4",
                 "FU_NH4",
                 "FD_NH4",
                 "FU_PO4",
                 "FD_PO4",
                 "FU_Fe",
                 "FD_Fe",
                 "FU_VivP",
                 "FD_VivP",
                 "FU_OrgP",
                 "FD_OrgP",
                 "FU_DIC",
                 "FD_DIC",
                 "FU_Mn",
                 "FD_Mn",
                 "FU_OrgN",
                 "FD_OrgN",
                 "FU_H2S",
                 "FD_H2S",
                 "FU_FeS",
                 "FD_FeS",
                 "FU_FeS2",
                 "FD_FeS2",
                 "FU_S0",
                 "FD_S0",
                 "FU_FeCO3",
                 "FD_FeCO3",
                 "FU_MnCO3",
                 "FD_MnCO3",
                 "FU_N2",
                 "FD_N2"
)

#################################################Creating data output#######################################################
##############################################################################################################
#fluxes and Ksp
FU_tran_OrgCA <- array(data=NA)  
FU_tran_OrgCB <- array(data=NA)  
FU_tran_O2    <- array(data=NA)  
FU_tran_NO3   <- array(data=NA)  
FU_tran_MnO2A <- array(data=NA)  
FU_tran_FeOH3A<- array(data=NA)  
FU_tran_FeP   <- array(data=NA)  
FU_tran_SO4   <- array(data=NA)  
FU_tran_CH4   <- array(data=NA)  
FU_tran_NH4   <- array(data=NA)  
FU_tran_PO4   <- array(data=NA)  
FU_tran_Fe    <- array(data=NA)  
FU_tran_VivP  <- array(data=NA)  
FU_tran_DIC   <- array(data=NA)  
FU_tran_Mn    <- array(data=NA)  
FU_tran_H2S   <- array(data=NA)  
FU_tran_FeS   <- array(data=NA)  
FU_tran_FeS2  <- array(data=NA)  
FU_tran_S0    <- array(data=NA)  
FU_tran_FeCO3 <- array(data=NA)  
FU_tran_MnCO3 <- array(data=NA)  
FU_tran_N2    <- array(data=NA)  

FD_tran_OrgCA <- array(data=NA)  
FD_tran_OrgCB <- array(data=NA)  
FD_tran_O2    <- array(data=NA)  
FD_tran_NO3   <- array(data=NA)  
FD_tran_MnO2A <- array(data=NA)  
FD_tran_FeOH3A<- array(data=NA)  
FD_tran_FeP   <- array(data=NA)  
FD_tran_SO4   <- array(data=NA)  
FD_tran_CH4   <- array(data=NA)  
FD_tran_NH4   <- array(data=NA)  
FD_tran_PO4   <- array(data=NA)  
FD_tran_Fe    <- array(data=NA)  
FD_tran_VivP  <- array(data=NA)  
FD_tran_DIC   <- array(data=NA)  
FD_tran_Mn    <- array(data=NA)  
FD_tran_H2S   <- array(data=NA)  
FD_tran_FeS   <- array(data=NA)  
FD_tran_FeS2  <- array(data=NA)  
FD_tran_S0    <- array(data=NA)  
FD_tran_FeCO3 <- array(data=NA)  
FD_tran_MnCO3 <- array(data=NA)  
FD_tran_N2    <- array(data=NA)  

tran_Ksp_FeCO3 <- array(data=NA)  
tran_Ksp_viv   <- array(data=NA) 

#Rates species
Int_rate_OrgCA <- array(data=NA)
Int_rate_OrgCB <- array(data=NA) 
Int_rate_O2 <- array(data=NA)    
Int_rate_NO3 <- array(data=NA)   
Int_rate_MnO2A <- array(data=NA) 
Int_rate_FeOH3A <- array(data=NA)
Int_rate_FeP <- array(data=NA)   
Int_rate_SO4 <- array(data=NA)   
Int_rate_CH4 <- array(data=NA)   
Int_rate_NH4 <- array(data=NA)   
Int_rate_PO4 <- array(data=NA)   
Int_rate_Fe <- array(data=NA)    
Int_rate_VivP <- array(data=NA)  
Int_rate_DIC <- array(data=NA)   
Int_rate_Mn <- array(data=NA)    
Int_rate_H2S <- array(data=NA)   
Int_rate_FeS <- array(data=NA)   
Int_rate_FeS2 <- array(data=NA)  
Int_rate_S0 <- array(data=NA)    
Int_rate_FeCO3 <- array(data=NA) 
Int_rate_MnCO3 <- array(data=NA) 
Int_rate_N2 <- array(data=NA)
Int_rate_omega_FeCO3 <- array(data=NA) 
Int_rate_omega_viv <- array(data=NA)

#rates reactions
Int_rate_R1a  <- array(data=NA) 
Int_rate_R1b  <- array(data=NA) 
Int_rate_RNa  <- array(data=NA)
Int_rate_RNb  <- array(data=NA) 
Int_rate_RMa  <- array(data=NA) 
Int_rate_RMb  <- array(data=NA) 
Int_rate_R2a_Ox  <- array(data=NA) 
Int_rate_R2b_Ox  <- array(data=NA)
Int_rate_R2a_P  <- array(data=NA) 
Int_rate_R2b_P  <- array(data=NA) 
Int_rate_R3a  <- array(data=NA) 
Int_rate_R3b  <- array(data=NA) 
Int_rate_R4a  <- array(data=NA) 
Int_rate_R4b  <- array(data=NA) 
Int_rate_R19  <- array(data=NA) 
Int_rate_R5_P  <- array(data=NA) 
Int_rate_R5_Ox  <- array(data=NA) 
Int_rate_R6  <- array(data=NA) 
Int_rate_R7  <- array(data=NA)
Int_rate_R8  <- array(data=NA) 
Int_rate_R10_Ox  <- array(data=NA) 
Int_rate_R10_P  <- array(data=NA)
Int_rate_R11  <- array(data=NA) 
Int_rate_R20  <- array(data=NA) 
Int_rate_R14  <- array(data=NA) 
Int_rate_R15  <- array(data=NA) 
Int_rate_R12  <- array(data=NA) 
Int_rate_R25  <- array(data=NA) 
Int_rate_R26  <- array(data=NA)
Int_rate_R27  <- array(data=NA) 
Int_rate_R28  <- array(data=NA) 
Int_rate_R29_Ox  <- array(data=NA)
Int_rate_R29_P  <- array(data=NA) 
Int_rate_R30  <- array(data=NA) 
Int_rate_R34  <- array(data=NA) 
Int_rate_precip_rate_FeCO3  <- array(data=NA) 
Int_rate_diss_rate_FeCO3  <- array(data=NA) 
Int_rate_precip_rate_viv  <- array(data=NA) 
Int_rate_diss_rate_viv  <- array(data=NA) 

#ratios
Int_rate_D_PO4  <- array(data=NA)
Int_rate_omega_FeCO3  <- array(data=NA)
Int_rate_omega_Viv  <- array(data=NA)
Int_rate_IAP_FeCO3  <- array(data=NA) 
Int_rate_IAP_Viv  <- array(data=NA) 

#concentrations
Int_conc_OrgCA<- array(data=NA) 
Int_conc_OrgCB<- array(data=NA)  
Int_conc_O2<- array(data=NA) 
Int_conc_NO3<- array(data=NA)  
Int_conc_MnO2A<- array(data=NA)  
Int_conc_FeOH3A<- array(data=NA)  
Int_conc_FeP<- array(data=NA)  
Int_conc_SO4<- array(data=NA)  
Int_conc_CH4<- array(data=NA) 
Int_conc_NH4<- array(data=NA)  
Int_conc_PO4<- array(data=NA)  
Int_conc_Fe<- array(data=NA)  
Int_conc_VivP<- array(data=NA)  
# Int_conc_OrgP<- array(data=NA)  
Int_conc_DIC<- array(data=NA)  
Int_conc_Mn<- array(data=NA)  
# Int_conc_OrgN<- array(data=NA)  
Int_conc_H2S<- array(data=NA)  
Int_conc_FeS<- array(data=NA)  
Int_conc_FeS2<- array(data=NA)  
Int_conc_S0<- array(data=NA)  
Int_conc_FeCO3<- array(data=NA) 
Int_conc_MnCO3<- array(data=NA)  
Int_conc_N2<- array(data=NA)  

