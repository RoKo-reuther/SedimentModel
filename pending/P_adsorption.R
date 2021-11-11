## implementation
# constants: bei shared constants reinhauen
  adcap_FeOH3 <- 0.27 # [molP/molFe] adsorption capacity of FeOH3
  k_adsP <- 365 # [yr-1] phosphate adsorption reaction rate
  k_desP <- 365 # [yr-1] phosphate desorption reaction rate
  kt_sorpP <- 1 # [-] temperature coefficient for phosphate sorption processes
  KadsP20 <- 3.1 # [m3_pw/molP] adsorption equilibrium constant at 20°C !!!determine value!!! deltares: 0.1 m³/gP -> 0.1 m³/gP * 31 gP/molP = 3.1 m³/molP
  
# shared regulation terms
  KadsP <- KadsP20*kt_sorpP**(TC-20) # [m3_pw/molP]
  ads_t <- FeOH3*adcap_FeOH3 # [mol/m3_sf]; differing adsorption capacity for FeOH3A and FeP, otherwise same adsorption behaviour
  ads_f <- ads_t - adsorbed_P # [mol/m3_sf]
  aP_e = ads_t*(KadsP*PO4)/(1+KadsP*PO4) # [mol/m3_sf]
  # actual phosphate load of FeOH3A/FeP [molP/molFe]
  phosphate_load_FeOH3A <- (adsorbed_P+1e-50)/(FeOH3A+1e-20)
  
# new pools
  adsorbed_P # [mol/m3_sf]
  
# ad/desorption rates    
  R_adsP <- k_adsP * (aP_e - adsorbed_P) * ifelse(aP_e > adsorbed_P, 1, 0) # [mol/V_sf/y] Langmuir equilibrium regulated adsorption
  R_desP1 <- -1 * k_desP * (aP_e - adsorbed_P) * ifelse(aP_e <= adsorbed_P, 1, 0) # [mol/V_sf/y] Langmuir equilibrium regulated desorption
  R_desP2 <- ifelse(RFeOH3 < 0, -1*RFeOH3A*phosphate_load_FeOH3A, 0) + ifelse(RFeP < 0, -1*RFeP*phosphate_load_FeP, 0) # [mol/V_sf/y] release of adsorbed phosphate through chemical reactions

  
## draw Langmuir isotherme
TC <- 10
FeOH3A <- 100
adcap_FeOH3 <- 0.01552698
# adcap calculation: Karel: 92.039 µmol/g
# (ads_t [mol_aP/m³_sf] / FeOH3A [mol_FeOH3A/m³_sf]) * MW_FeOH3A^-1 [g/mol_FeOH3A] * 1e6 µmol/mol = 92.039 µmol/g
# ads_t [mol_aP/m³_sf] / FeOH3A [mol_FeOH3A/m³_sf] = adcap_FeOH3A [mol_aP/mol_FeOH3A]
# adcap_FeOH3A [mol_aP/mol_FeOH3A] = 92.039 µmol/g * MW_FeOH3A [g/mol_FeOH3A] * 1e-6 [mol/µmol]
# with 168.7 g/mol (ferrihydrite)
# 92.039 µmol/g * 168.7 g/mol * 1e-6 mol/µmol = 0.01552698 mol_aP/mol_FeOH3A
ads_t <- FeOH3A*adcap_FeOH3
  
aPe <- function(PO4){
  KadsP20 <- 143 # Karel: 0.143 L/µmol * 1e-3 m³/L * 1e6 µmol/mol = 143 m³/mol
  kt_sorpP <- 1
  KadsP <- KadsP20*kt_sorpP**(TC-20)
  aP_e <- ads_t*(KadsP*PO4)/(1+KadsP*PO4)
  return(aP_e)
}
  
curve(aPe, from=0, to=0.1, xlab = "PO4 (mol/m3)", ylab = "aPe (mol/m3)")
segments(-0.01, 1.150445, 0.02, 1.150445, col="red")
segments(0.02, 1.150445, 0.02, -0.1, col = "red")
segments(-0.01, 0.98, 0.02, 0.98, col="blue")
segments(0.02, 0.98, 0.02, -0.1, col = "blue")
