# constants: bei shared constants reinhauen
  adcap_FeOH3 <- 0.27 # [molP/molFe] adsorption capacity of FeOH3
  adcap_FeP <- 0 # [molP/molFe] adsorption capacity of FeP (FeOH3 coprecipitated with PO4) !!!???!!!
  k_adsP <- 365 # [yr-1] phosphate adsorption reaction rate
  k_desP <- 365 # [yr-1] phosphate desorption reaction rate
  kt_sorpP <- 1 # [-] temperature coefficient for phosphate sorption processes
  KadsP20 <- 3.1 # [m3_pw/molP] adsorption equilibrium constant at 20°C !!!determine value!!! deltares: 0.1 m³/gP -> 0.1 m³/gP * 31 gP/molP = 3.1 m³/molP
  
# shared regulation terms
  KadsP <- KadsP20*kt_sorpP**(TC-20) # [m3_pw/molP]
  ads_t <- FeOH3*adcap_FeOH3 + FeP*adcap_FeP # [mol/m3_sf]; differing adsorption capacity for FeOH3A and FeP, otherwise same adsorption behaviour
  ads_f <- ads_t - adsorbed_P # [mol/m3_sf]
  aP_e = ads_f*(KadsP*PO4)/(1+KadsP*PO4) # [mol/m3_sf]
  # actual phosphate load of FeOH3A/FeP [molP/molFe]
  phosphate_load_FeOH3A <- (adsorbed_P/(FeOH3A*adcap_FeOH3A*ratio_Fe + FeP*adcap_FeP*(1-ratio_Fe)))*(adcap_FeOH3A*ratio_Fe)
  phosphate_load_FeP <- (adsorbed_P/(FeOH3A*adcap_FeOH3A*ratio_Fe + FeP*adcap_FeP*(1-ratio_Fe)))*(adcap_FeP*(1-ratio_Fe))
  
# new pools
  adsorbed_P # [mol/m3_sf]
  
# ad/desorption rates    
  R_adsP <- k_adsP * (aP_e - adsorbed_P) * ifelse(aP_e > adsorbed_P, 1, 0) # [mol/V_sf/y] Langmuir equilibrium regulated adsorption
  R_desP1 <- -1 * k_desP * (aP_e - adsorbed_P) * ifelse(aP_e <= adsorbed_P, 1, 0) # [mol/V_sf/y] Langmuir equilibrium regulated desorption
  R_desP2 <- ifelse(RFeOH3 < 0, -1*RFeOH3A*phosphate_load_FeOH3A, 0) + ifelse(RFeP < 0, -1*RFeP*phosphate_load_FeP, 0) # [mol/V_sf/y] release of adsorbed phosphate through chemical reactions

