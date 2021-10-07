
###########################################################################################################
#                                       CONFIGURE CHEMICAL BASE HERE                                      #
###########################################################################################################

# define species / pools
# create an entry in the following form for every species considered
# NOTE: A species can have subspecies (e.g. different reactivities)
  # list them in a subspecies sublist of the species and give subspecies a name (a, b, c, ...)
  # if a reaction should occcour at different rates for subspecies, you have to define that in the reaction description
  # -> add a 'subsp_def=c()' vector in the reaction description...
  # ... and name the different reaction rates in the following form: 'reaction_rate_namea', 'reaction_rate_nameb', ... (e.g. R1_a, R1_b,, R1_c)
  # if one reaction has differing reaction rates for subspecies of more than one species, you can define it in this way:
  # subsp_def=c("sp1", "sp2"), where sp1 and sp2 have subspecies sp1a, sp1b, sp2a, sp2b
  # equations=list(
    # RX_ab="k_1 * sp1a * sp2b",
    # RX_ba="k_2 * sp1b * sp2a"
  # -> note, that the order of mentioning the species in "subsp_def" indicates the order of defining the subspecies in the reaction rate names affix
  # allows to a) let reactions occur at different rates for subspecies b) "devide" production rate of a reaction to different subspecies by using factors
# "dummy_species"=list(abbreviation="d_s", name="dummy species", {subspecies=list("subsp1", "subsp2"), phase="solid / solute", activated=TRUE)

species_collection <- list(
  list(abbreviation="OM", name="organic matter", subspecies=list(a="OrgCA", b="OrgCB", c="OrgCC"), phase="solid", activated=TRUE),
  list(abbreviation="O2", name="oxygen", abbr_diffcoeff="O2", phase="solute", activated=TRUE),
  list(abbreviation="NH4", name="ammonium", abbr_diffcoeff="NH4", phase="solute", activated=TRUE),
  list(abbreviation="PO4", name="phosphate (pool)", abbr_diffcoeff="PO4", phase="solute", activated=TRUE),
  list(abbreviation="NO3", name="nitrate", abbr_diffcoeff="NO3", phase="solute", activated=TRUE),
  list(abbreviation="N2", name="nitrogen", abbr_diffcoeff="N2", phase="solute", activated=TRUE),
  list(abbreviation="MnO2", name="manganese di-oxide", subspecies=list(a="MnO2A"), phase="solid", activated=TRUE),
  list(abbreviation="Mn_2", name="manganese", abbr_diffcoeff="Mn", phase="solute", activated=TRUE),
  list(abbreviation="MnCO3", name="manganese carbonate", phase="solid", activated=TRUE),
  list(abbreviation="Fe(OH)3", name="iron hydroxide", subspecies=list(a="FeOH3A"), phase="solid", activated=TRUE),
  list(abbreviation="Fe_2", name="iron", abbr_diffcoeff="Fe", phase="solute", activated=TRUE),
  list(abbreviation="FeCO3", name="iron carbonate (siderite)", phase="solid", activated=TRUE),
  list(abbreviation="VivP", name="ferrous phosphate (vivianite)", phase="solid", activated=TRUE),
  list(abbreviation="FeS", name="iron mono-sulphide", phase="solid", activated=TRUE),
  list(abbreviation="FeS2", name="iron di-sulphide (pyrite)", phase="solid", activated=TRUE),
  list(abbreviation="S0", name="elemental sulphur", phase="solid", activated=TRUE),
  list(abbreviation="SO4", name="sulphate", abbr_diffcoeff="SO4", phase="solute", activated=TRUE),
  list(abbreviation="H2S", name="hydrogen sulphate (pool)", abbr_diffcoeff="H2S", phase="solute", activated=TRUE),
  list(abbreviation="CH4", name="methane", abbr_diffcoeff="CH4", phase="solute", activated=TRUE),
  list(abbreviation="DIC", name="dissolved inorganic carbon", abbr_diffcoeff="HCO3", phase="solute", activated=TRUE),
  list(abbreviation="adsorbed_P", name="iron adsorbed phosphorous", phase="solid", activated=TRUE) # has to be listed after adsorbens!
)


# define "shared" reaction rate constants and other constants, that are used in more than one reaction: aim: have one place to change value
# e.g. all OM-degradation reactions use the same decay constants and the same limiting saturation concentrations
shared_reaction_constants <- list(
  ## OM degradation
  k_alpha=list(value=0.05, u_unit="/y"),
  k_beta=list(value=0.0086, u_unit="/y"),
  K_mO2=list(value=1e-3, u_unit="mol/m3_pw"),
  K_mNO3=list(value=4e-3, u_unit="mol/m3_pw"),
  K_mMnO2=list(value=20e-2*dens_dw, u_unit="mol/m3_sf"),
  K_mFeOH3=list(value=65e-2*dens_dw, u_unit="mol/m3_sf"),
  K_mSO4=list(value=1.6e-3, u_unit="mol/m3_pw"),
  
  ## solubility products
  Ksp_FeCO3=list(value=10^(-0.8), u_unit="-"), # equilibrium solubility for FeCO3 formation (reaction) at  equilibrium, estimated for ph =approx 7
  #Ksp_viv=list(value=3.98e-10, u_unit="-"), # equilibrium solubility for vivanite formation (reaction) at  equilibrium, estimated for ph =approx 7; original: 10^(-24,4)!
  
  ## temperature constants: k=k_20*kt^(TC-20)
  kt_decomp=list(value=1.047, u_unit="-"),  # kt_decomp for decomposition of OM
  kt_microbial=list(value=1.07, u_unit="-"), # kt_microbial for microbial processes except decomposition of OM
  kt_sorpP=list(value=1, u_unit="-"), # temperature coefficient for phosphate sorption processes
  
  ## PO4 adsorption
  adcap_FeOH3A=list(value=0.27, u_unit="molP/molFe"), # adsorption capacity of FeOH3
  KadsP20=list(value=3.1, u_unit="m3_pw/molP") # adsorption equilibrium constant at 20°C !!!determine value!!! deltares: 0.1 m³/gP -> 0.1 m³/gP * 31 gP/molP = 3.1 m³/molP
)


# define shared chemical regulation terms (e.g. Michaelis-Menten approaches, saturation indices, summing pools ...)
# if a regulation term (a) is calculated with the help of another regulation term (b), b must appear before a in the list
shared_regulation_terms <- list(
  ## ionic activity products and omegas
  IAP_FeCO3="Fe_2 * DIC",
  omega_FeCO3="IAP_FeCO3/Ksp_FeCO3",
  IAP_viv="(Fe_2**3) * (PO4**2)",
  omega_viv="IAP_viv/Ksp_viv",
  
  ## temperature-correction-factor for reaction constants
  tempcorr_decomp="kt_decomp**(TC-20)", # for decomposition of OM
  tempcorr_microbial="kt_microbial**(TC-20)", # for microbial processes except decomposition of OM
  
  ## PO4 adsorption (Langmuir)
  KadsP="KadsP20*kt_sorpP**(TC-20)", # [m3_pw/molP]
  ads_t="FeOH3A*adcap_FeOH3A", # [mol/m3_sf]; total adsorption sites
  aP_e="ads_t*(KadsP*PO4)/(1+KadsP*PO4)", # [mol/m3_sf]; adsorbed phosphate at equilibrium
  phosphate_load_FeOH3A="ifelse(FeOH3A==0, 0, adsorbed_P/FeOH3A)" #"(adsorbed_P+1e-50)/(FeOH3A+1e-20)"  # actual phosphate load of FeOH3A [molP/molFe]
  # if more than one "adsorbens-specie" the phosphate load for e.g. one of two species is
  # phosphate_load_S1 = (adsorbed_P/(S1*adcap_S1*(S1/(S1 + S2)) + (S2*adcap_S2*(S2/(S1 + S2)))) * (adcap_S1*(S1/(S2 + S2)))
)


# define reactions
# create an list entry in the following form for every reaction considered; reagents and products have to named by defined abbreviation (see section above)
# E_dummy=list(abbreviation="E_dummy",
#              name="dummy reaction",
#              involved_species=list(
#                educts=list("E1"=list(abbreviation="E1", stoic=1), "E2" = list(abbreviation="E2", stoic=1)),
#                products=list("P1"=list(abbreviation="P1", stoic=1), "P2" = list(abbreviation="P2", stoic=1))),
#              reaction_rate_constants=list(k0=list(value=1 , u_unit="your unit")),
#              reaction_rates=list(equations=list(R0="k0 * E1 * E2"), u_unit="mol/V_pw/y / mol/V_sf/y"),
#              subsp_def="speciesX",
#              activated=TRUE)

reactions_collection <- list(
  E1=list(abbreviation="E1",
          name="Aerobic OM mineralisation",
          involved_species=list(
            educts=list(
              "OM"=list(abbreviation="OM", stoic=1),
              "O2" = list(abbreviation="O2", stoic=1)), 
            products=list(
              "DIC"=list(abbreviation="DIC", stoic=1),
              "NH4"=list(abbreviation="NH4", stoic=1*(1/CtoN)),
              "PO4"=list(abbreviation="PO4", stoic=1*(1/CtoP)))),
          reaction_rate_constants=list(), # only use shared ones
          shared_terms=list("tempcorr_decomp"),
          reaction_rates=list(
            equations=list(
              R1_a="k_alpha * tempcorr_decomp * OrgCA * O2 / (K_mO2 + O2)",
              R1_b="k_beta * tempcorr_decomp  * OrgCB * O2 / (K_mO2 + O2)"),
            u_unit="mol/V_sf/y"),
          subsp_def=c("OM"),
          activated=TRUE),
  
  E2=list(abbreviation="E2",
          name="Denitrification coupled to OM degradation",
          involved_species=list(
            educts=list(
              "OM"=list(abbreviation="OM", stoic=1),
              "NO3" = list(abbreviation="NO3", stoic=0.8)), 
            products=list(
              "DIC"=list(abbreviation="DIC", stoic=1),
              "NH4"=list(abbreviation="NH4", stoic=1*(1/CtoN)),
              "PO4"=list(abbreviation="PO4", stoic=1*(1/CtoP)),
              "N2"=list(abbreviation="N2", stoic=0.4))),
          reaction_rate_constants=list(), # only use shared ones
          shared_terms=list("tempcorr_decomp"),
          reaction_rates=list(
            equations=list(
              RN_a="k_alpha * tempcorr_decomp * OrgCA  * (NO3 / (K_mNO3 + NO3)) * (K_mO2 / (K_mO2 + O2))",
              RN_b="k_beta * tempcorr_decomp  * OrgCB  * (NO3 / (K_mNO3 + NO3)) * (K_mO2 / (K_mO2 + O2))"),
            u_unit="mol/V_sf/y"),
          subsp_def=c("OM"),
          activated=TRUE),
  
  E3=list(abbreviation="E3",
          name="MnO2 reduction coupled to OM degradation",
          involved_species=list(
            educts=list(
              "OM"=list(abbreviation="OM", stoic=1),
              "MnO2" = list(abbreviation="MnO2", stoic=2)), 
            products=list(
              "DIC"=list(abbreviation="DIC", stoic=1),
              "NH4"=list(abbreviation="NH4", stoic=1*(1/CtoN)),
              "PO4"=list(abbreviation="PO4", stoic=1*(1/CtoP)),
              "Mn_2"=list(abbreviation="Mn_2", stoic=2))),
          reaction_rate_constants=list(), # only use shared ones
          shared_terms=list("tempcorr_decomp"),
          reaction_rates=list(
            equations=list(
              RM_aa="k_alpha * tempcorr_decomp * OrgCA * (MnO2A / (K_mMnO2 + MnO2A)) * (K_mNO3 / (K_mNO3 + NO3)) * (K_mO2 / (K_mO2 + O2))",
              RM_ba="k_beta * tempcorr_decomp  * OrgCB * (MnO2A / (K_mMnO2 + MnO2A)) * (K_mNO3 / (K_mNO3 + NO3)) * (K_mO2 / (K_mO2 + O2))"),
            u_unit="mol/V_sf/y"),
          subsp_def=c("OM", "MnO2"),
          activated=TRUE),
  
  E4=list(abbreviation="E4",
           name="FeOx reduction coupled to OM degradation",
           involved_species=list(
             educts=list(
               "OM"=list(abbreviation="OM", stoic=1),
               "Fe(OH)3" = list(abbreviation="Fe(OH)3", stoic=4)), 
             products=list(
               "DIC"=list(abbreviation="DIC", stoic=1),
               "NH4"=list(abbreviation="NH4", stoic=1*(1/CtoN)),
               "PO4"=list(abbreviation="PO4", stoic=1*(1/CtoP)),
               "Fe_2"=list(abbreviation="Fe_2", stoic=4))),
           reaction_rate_constants=list(), # only use shared ones
           shared_terms=list("tempcorr_decomp"),
           reaction_rates=list(
             equations=list(
               R2_Ox_aa="k_alpha * tempcorr_decomp * OrgCA * (FeOH3A / (K_mFeOH3 + FeOH3A)) * (K_mMnO2 / (K_mMnO2 + MnO2A)) * (K_mNO3 / (K_mNO3 + NO3)) * (K_mO2 / (K_mO2 + O2))",
               R2_Ox_ba="k_beta * tempcorr_decomp  * OrgCB * (FeOH3A / (K_mFeOH3 + FeOH3A)) * (K_mMnO2 / (K_mMnO2 + MnO2A)) * (K_mNO3 / (K_mNO3 + NO3)) * (K_mO2 / (K_mO2 + O2))"),
             u_unit="mol/V_sf/y"),
           subsp_def=c("OM", "Fe(OH)3"),
           activated=TRUE),
  
  E5=list(abbreviation="E5",
          name="Sulfate reduction coupled to OM degradation",
          involved_species=list(
            educts=list(
              "OM"=list(abbreviation="OM", stoic=1),
              "SO4" = list(abbreviation="SO4", stoic=0.5)), 
            products=list(
              "DIC"=list(abbreviation="DIC", stoic=1),
              "NH4"=list(abbreviation="NH4", stoic=1*(1/CtoN)),
              "PO4"=list(abbreviation="PO4", stoic=1*(1/CtoP)),
              "H2S"=list(abbreviation="H2S", stoic=0.5))),
          reaction_rate_constants=list(), # only use shared ones
          shared_terms=list("tempcorr_decomp"),
          reaction_rates=list(
            equations=list(
              R3_a="k_alpha * tempcorr_decomp * OrgCA * (SO4 / (K_mSO4 + SO4)) * (K_mFeOH3 / (K_mFeOH3 + FeOH3A)) * (K_mMnO2 / (K_mMnO2 + MnO2A)) * (K_mNO3 / (K_mNO3 + NO3)) * (K_mO2 / (K_mO2 + O2))",
              R3_b="k_beta * tempcorr_decomp  * OrgCB * (SO4 / (K_mSO4 + SO4)) * (K_mFeOH3 / (K_mFeOH3 + FeOH3A)) * (K_mMnO2 / (K_mMnO2 + MnO2A)) * (K_mNO3 / (K_mNO3 + NO3)) * (K_mO2 / (K_mO2 + O2))"),
            u_unit="mol/V_sf/y"),
          subsp_def=c("OM"),
          activated=TRUE),
  
  E6=list(abbreviation="E6",
          name="Methanogenesis",
          involved_species=list(
            educts=list(
              "OM"=list(abbreviation="OM", stoic=1)),
            products=list(
              "DIC"=list(abbreviation="DIC", stoic=0.5),
              "NH4"=list(abbreviation="NH4", stoic=1*(1/CtoN)),
              "PO4"=list(abbreviation="PO4", stoic=1*(1/CtoP)),
              "CH4"=list(abbreviation="CH4", stoic=0.5))),
          reaction_rate_constants=list(), # only use shared ones
          shared_terms=list("tempcorr_decomp"),
          reaction_rates=list(
            equations=list(
              R4_a="k_alpha * tempcorr_decomp * OrgCA * (K_mSO4 / (K_mSO4 + SO4)) * (K_mFeOH3 / (K_mFeOH3 + FeOH3A)) * (K_mMnO2 / (K_mMnO2 + MnO2A)) * (K_mNO3 / (K_mNO3 + NO3)) * (K_mO2 / (K_mO2 + O2))",
              R4_b="k_beta * tempcorr_decomp  * OrgCB * (K_mSO4 / (K_mSO4 + SO4)) * (K_mFeOH3 / (K_mFeOH3 + FeOH3A)) * (K_mMnO2 / (K_mMnO2 + MnO2A)) * (K_mNO3 / (K_mNO3 + NO3)) * (K_mO2 / (K_mO2 + O2))"),
            u_unit="mol/V_sf/y"),
          subsp_def=c("OM"),
          activated=TRUE),
  
  E8=list(abbreviation="E8",
          name="Nitrification",
          involved_species=list(
            educts=list(
              "O2"=list(abbreviation="O2", stoic=2),
              "NH4"=list(abbreviation="NH4", stoic=1),
              "DIC"=list(abbreviation="DIC", stoic=2)),
            products=list(
              "NO3"=list(abbreviation="NO3", stoic=1),
              "DIC"=list(abbreviation="DIC", stoic=2))),
          reaction_rate_constants=list(k1=list(value=5e3 , u_unit="m3 mol-1 y-1")),
          shared_terms=list("tempcorr_microbial"),
          reaction_rates=list(equations=list(R19="k1 * tempcorr_microbial * O2 * NH4"), u_unit="mol/V_pw/y"),
          activated=TRUE),
  
  E9=list(abbreviation="E9",
           name="Fe(OH)3 formation",
           involved_species=list(
             educts=list(
               "O2"=list(abbreviation="O2", stoic=1),
               "Fe_2"=list(abbreviation="Fe_2", stoic=4)),
             products=list(
               "Fe(OH)3"=list(abbreviation="Fe(OH)3", stoic=4))),
           reaction_rate_constants=list(k2=list(value=1.4e5*1e-3 , u_unit="m3 mol-1 y-1")),
           shared_terms=list("tempcorr_microbial"),
           reaction_rates=list(
             equations=list(
               R5_Ox_a="k2 * tempcorr_microbial * O2 * Fe_2"), u_unit="mol/V_pw/y"),
           subsp_def=c("Fe(OH)3"),
           activated=TRUE),
  
  E12=list(abbreviation="E12",
           name="H2S oxidation",
           involved_species=list(
             educts=list(
               "O2"=list(abbreviation="O2", stoic=2),
               "H2S"=list(abbreviation="H2S", stoic=1)),
             products=list(
               "SO4"=list(abbreviation="SO4", stoic=1))),
           reaction_rate_constants=list(k5=list(value=1.6e2 , u_unit="m3 mol-1 y-1")),
           shared_terms=list("tempcorr_microbial"),
           reaction_rates=list(equations=list(R8="k5 * tempcorr_microbial * O2 * H2S"), u_unit="mol/V_pw/y"),
           activated=TRUE),
  
  E13=list(abbreviation="E13",
          name="Aerobic methane oxidation",
          involved_species=list(
            educts=list(
              "O2"=list(abbreviation="O2", stoic=2),
              "CH4"=list(abbreviation="CH4", stoic=1)),
            products=list(
              "DIC"=list(abbreviation="DIC", stoic=1))),
          reaction_rate_constants=list(k6=list(value=10e7 , u_unit="m3 mol-1 y-1")),
          shared_terms=list("tempcorr_microbial"),
          reaction_rates=list(equations=list(R9="k6 * tempcorr_microbial * O2 * CH4"), u_unit="mol/V_pw/y"),
          activated=TRUE),
  
  E14=list(abbreviation="E14",
             name="FeOx-reduction coupled to sulphide oxidation",
             involved_species=list(
               educts=list(
                 "Fe(OH)3"=list(abbreviation="Fe(OH)3", stoic=2),
                 "H2S"=list(abbreviation="H2S", stoic=1)),
               products=list(
                 "Fe_2"=list(abbreviation="Fe_2", stoic=2),
                 "S0"=list(abbreviation="S0", stoic=1))),
             reaction_rate_constants=list(k7=list(value=8 , u_unit="m3 mol-1 y-1")),
             shared_terms=list("tempcorr_microbial"),
             reaction_rates=list(
               equations=list(R10_Ox_a="k7 * tempcorr_microbial * FeOH3A * H2S"),
               u_unit="mol/V_sf/y"),
             subsp_def=c("Fe(OH)3"),
             activated=TRUE),
  
  E15=list(abbreviation="E15",
           name="FeS formation",
           involved_species=list(
             educts=list(
               "Fe_2"=list(abbreviation="Fe_2", stoic=1),
               "H2S"=list(abbreviation="H2S", stoic=1)),
             products=list(
               "FeS"=list(abbreviation="FeS", stoic=1))),
           reaction_rate_constants=list(k9=list(value=1.482e-1, u_unit="m3 mol-1 y-1")),
           shared_terms=list("tempcorr_microbial"),
           reaction_rates=list(equations=list(R11="k9 * tempcorr_microbial * Fe_2 * H2S"), u_unit="mol/V_pw/y"),
           activated=TRUE),
  
  E19=list(abbreviation="E19",
           name="SO4-reduction coupled to AOM",
           involved_species=list(
             educts=list(
               "SO4"=list(abbreviation="SO4", stoic=1),
               "CH4"=list(abbreviation="CH4", stoic=1),
               "DIC"=list(abbreviation="DIC", stoic=1)),
             products=list(
               "DIC"=list(abbreviation="DIC", stoic=2),
               "H2S"=list(abbreviation="H2S", stoic=1))),
           reaction_rate_constants=list(k13=list(value=10, u_unit="m3 mol-1 y-1")),
           shared_terms=list("tempcorr_microbial"),
           reaction_rates=list(equations=list(R12="k13 * tempcorr_microbial * SO4 * CH4"), u_unit="mol/V_pw/y"),
           activated=TRUE),
  
  E27=list(abbreviation="E27",
           name="MnCO3 precipitation",
           involved_species=list(
             educts=list(
               "Mn_2"=list(abbreviation="Mn_2", stoic=1),
               "DIC"=list(abbreviation="DIC", stoic=1)),
             products=list(
               "MnCO3"=list(abbreviation="MnCO3", stoic=1))),
           reaction_rate_constants=list(k23=list(value=2.65e-1, u_unit="m3 mol-1 y-1")),
           reaction_rates=list(equations=list(R27="k23 * Mn_2 * DIC*0.1"), u_unit="mol/V_pw/y"), #Note that CO3 is ~10% of DIC
           activated=TRUE),
  
  E28=list(abbreviation="E28",
           name="Mn oxidation",
           involved_species=list(
             educts=list(
               "Mn_2"=list(abbreviation="Mn_2", stoic=2),
               "O2"=list(abbreviation="O2", stoic=2)),
             products=list(
               "MnO2"=list(abbreviation="MnO2", stoic=2))),
           reaction_rate_constants=list(k24=list(value=1, u_unit="m3 mol-1 y-1")),
           shared_terms=list("tempcorr_microbial"),
           reaction_rates=list(
             equations=list(
               R28_a="k24 * tempcorr_microbial * Mn_2 * O2"),
             u_unit="mol/V_pw/y"),
           subsp_def=c("MnO2"),
           activated=TRUE),
  
  E29=list(abbreviation="E29",
             name="MnO2 reduction coupled to Fe oxidation",
             involved_species=list(
               educts=list(
                 "MnO2"=list(abbreviation="MnO2", stoic=1),
                 "Fe_2"=list(abbreviation="Fe_2", stoic=2)),
               products=list(
                 "Mn_2"=list(abbreviation="Mn_2", stoic=1),
                 "Fe(OH)3"=list(abbreviation="Fe(OH)3", stoic=2))),
             reaction_rate_constants=list(k25=list(value=23.652, u_unit="m3 mol-1 y-1")),
             shared_terms=list("tempcorr_microbial"),
             reaction_rates=list(
               equations=list(
                 R29_Ox_aa="k25 * tempcorr_microbial * MnO2A * Fe_2"),
               u_unit="mol/V_sf/y"),
             subsp_def=c("MnO2", "Fe(OH)3"),
             activated=TRUE),
  
  E30=list(abbreviation="E30",
            name="MnO2 reduction coupled to S oxidation",
            involved_species=list(
              educts=list(
                "MnO2"=list(abbreviation="MnO2", stoic=1),
                "H2S"=list(abbreviation="H2S", stoic=1)),
              products=list(
                "Mn_2"=list(abbreviation="Mn_2", stoic=1),
                "S0"=list(abbreviation="S0", stoic=1))),
            reaction_rate_constants=list(k27=list(value=4e4, u_unit="m3 mol-1 y-1")),
            shared_terms=list("tempcorr_microbial"),
            reaction_rates=list(
              equations=list(
                R30_a="k27 * tempcorr_microbial * MnO2A * H2S"),
              u_unit="mol/V_sf/y"),
            subsp_def=c("MnO2"),
            activated=TRUE),
  
  E32=list(abbreviation="E32",
            name="MnO2-reduction coupled AOM",
            involved_species=list(
              educts=list(
                "MnO2"=list(abbreviation="MnO2", stoic=4),
                "CH4"=list(abbreviation="CH4", stoic=1)),
              products=list(
                "Mn_2"=list(abbreviation="Mn_2", stoic=4),
                "DIC"=list(abbreviation="DIC", stoic=1))),
            reaction_rate_constants=list(k30=list(value=1.7e-3, u_unit="m3 mol-1 y-1")),
            shared_terms=list("tempcorr_microbial"),
            reaction_rates=list(
              equations=list(
                R34_a="k30 * tempcorr_microbial * MnO2A * CH4"),
              u_unit="mol/V_sf/y"),
            subsp_def=c("MnO2"),
            activated=TRUE),
  
  E22=list(abbreviation="E22",
           name="FeCO3 formation",
           involved_species=list(
             educts=list(
               "Fe_2"=list(abbreviation="Fe_2", stoic=1),
               "DIC"=list(abbreviation="DIC", stoic=1)),
             products=list(
               "FeCO3"=list(abbreviation="FeCO3", stoic=1))),
           reaction_rate_constants=list(k18=list(value=7*2.7*1e-3, u_unit="m3 mol-1 y-1")),
           shared_terms=list("IAP_FeCO3", "omega_FeCO3"),
           # FeCO3 precipitation occurs when omega>1 at a rate kp*(omega-1) and does not precipitate if in equilibirum, needs to be oversaturated
           reaction_rates=list(equations=list(precip_rate_FeCO3="k18*Fe_2*DIC*ifelse(omega_FeCO3 > 1, (omega_FeCO3-1), 0)"), u_unit="mol/V_pw/y"),
           activated=TRUE),
  
  E33=list(abbreviation="E33",
           name="FeCO3 dissolution",
           involved_species=list(
             educts=list(
               "FeCO3"=list(abbreviation="FeCO3", stoic=1)),
             products=list(
               "Fe_2"=list(abbreviation="Fe_2", stoic=1),
               "DIC"=list(abbreviation="DIC", stoic=1))),
           reaction_rate_constants=list(k32=list(value=7*2.7*1e-3, u_unit="y-1")),
           shared_terms=list("IAP_FeCO3", "omega_FeCO3"),
           # FeCO3 dissolution occurs when omega<1, at a rate proportional to FeCO3 concentration and does not dissolute if no solid phase is present
           reaction_rates=list(equations=list(diss_rate_FeCO3="-k32*FeCO3*ifelse(omega_FeCO3 <= 1, (omega_FeCO3-1), 0)"), u_unit="mol/V_sf/y"),
           activated=TRUE),
  
  E24=list(abbreviation="E24",
           name="vivianite formation",
           involved_species=list(
             educts=list(
               "Fe_2"=list(abbreviation="Fe_2", stoic=3),
               "PO4"=list(abbreviation="PO4", stoic=2)),
             products=list(
               "VivP"=list(abbreviation="VivP", stoic=1))),
           reaction_rate_constants=list(k20=list(value=1.15e-1, u_unit="m3 mol-1 y-1")), # 1.15e-1
           shared_terms=list("IAP_viv", "omega_viv"),
           # Vivianite precipitation occurs when omega>1 at a rate kp*(omega-1) and does not precipitate if in equilibirum, needs to be oversaturated
           reaction_rates=list(equations=list(precip_rate_viv="k20*Fe_2*PO4*ifelse(omega_viv > 1, (omega_viv-1), 0)"), u_unit="mol/V_pw/y"),
           activated=FALSE),
  
  E25=list(abbreviation="E25",
           name="vivianite dissolution",
           involved_species=list(
             educts=list(
               "VivP"=list(abbreviation="VivP", stoic=1)),
             products=list(
               "Fe_2"=list(abbreviation="Fe_2", stoic=3),
               "PO4"=list(abbreviation="PO4", stoic=2))),
           reaction_rate_constants=list(k21=list(value=1.15e-1, u_unit="y-1")),
           shared_terms=list("IAP_viv", "omega_viv"),
           # vivianite dissolution occurs when omega<1, at a rate proportional to vivianite concentration and does not dissolute if no viv is present
           reaction_rates=list(equations=list(diss_rate_viv="-k21*VivP*ifelse(omega_viv < 1, (omega_viv-1), 0)"), u_unit="mol/V_sf/y"),
           activated=FALSE),
  
  E26=list(abbreviation="E26",
           name="Conversion of vivianite to FeS",
           involved_species=list(
             educts=list(
               "VivP"=list(abbreviation="VivP", stoic=1),
               "H2S"=list(abbreviation="H2S", stoic=3)),
             products=list(
               "FeS"=list(abbreviation="FeS", stoic=3),
               "PO4"=list(abbreviation="PO4", stoic=2))),
           reaction_rate_constants=list(k22=list(value=8e-4, u_unit="m3 mol-1 y-1")),
           shared_terms=list("tempcorr_microbial"),
           reaction_rates=list(equations=list(R26="k22 * tempcorr_microbial * VivP * H2S"), u_unit="mol/V_sf/y"),
           activated=FALSE),
  
  E10=list(abbreviation="E10",
           name="FeS dissolution ",
           involved_species=list(
             educts=list(
               "O2"=list(abbreviation="O2", stoic=2),
               "FeS"=list(abbreviation="FeS", stoic=1)),
             products=list(
               "SO4"=list(abbreviation="SO4", stoic=1),
               "Fe_2"=list(abbreviation="Fe_2", stoic=1))),
           reaction_rate_constants=list(k3=list(value=6e1, u_unit="m3 mol-1 y-1")),
           shared_terms=list("tempcorr_microbial"),
           reaction_rates=list(equations=list(R6="k3 * tempcorr_microbial * O2 * FeS"), u_unit="mol/V_sf/y"),
           activated=TRUE),
  
  E11=list(abbreviation="E11",
           name="FeS2 dissolution",
           involved_species=list(
             educts=list(
               "O2"=list(abbreviation="O2", stoic=7),
               "FeS2"=list(abbreviation="FeS2", stoic=2)),
             products=list(
               "SO4"=list(abbreviation="SO4", stoic=4),
               "Fe_2"=list(abbreviation="Fe_2", stoic=2))),
           reaction_rate_constants=list(k4=list(value=5e3, u_unit="m3 mol-1 y-1")),
           shared_terms=list("tempcorr_microbial"),
           reaction_rates=list(equations=list(R7="k4 * tempcorr_microbial * O2 * FeS2"), u_unit="mol/V_sf/y"),
           activated=TRUE),
  
  E16=list(abbreviation="E16",
           name="Pyrite formation",
           involved_species=list(
             educts=list(
               "FeS"=list(abbreviation="FeS", stoic=1),
               "H2S"=list(abbreviation="H2S", stoic=1)),
             products=list(
               "FeS2"=list(abbreviation="FeS2", stoic=1))),
           reaction_rate_constants=list(k10=list(value=3e-2, u_unit="m3 mol-1 y-1")),
           shared_terms=list("tempcorr_microbial"),
           reaction_rates=list(equations=list(R20="k10 * tempcorr_microbial * FeS * H2S"), u_unit="mol/V_sf/y"),
           activated=TRUE),
  
  E17=list(abbreviation="E17",
           name="S0 transformation into SO4 and H2S",
           involved_species=list(
             educts=list(
               "S0"=list(abbreviation="S0", stoic=4)),
             products=list(
               "H2S"=list(abbreviation="H2S", stoic=3),
               "SO4"=list(abbreviation="SO4", stoic=1))),
           reaction_rate_constants=list(k11=list(value=1.6e2, u_unit="y-1")),
           shared_terms=list("tempcorr_microbial"),
           reaction_rates=list(equations=list(R14="k11 * tempcorr_microbial * S0"), u_unit="mol/V_sf/y"),
           activated=TRUE),
  
  E18=list(abbreviation="E18",
           name="Pyrite formation",
           involved_species=list(
             educts=list(
               "FeS"=list(abbreviation="FeS", stoic=1),
               "S0"=list(abbreviation="S0", stoic=1)),
             products=list(
               "FeS2"=list(abbreviation="FeS2", stoic=1))),
           reaction_rate_constants=list(k12=list(value=7.258e2, u_unit="m3 mol-1 y-1")),
           shared_terms=list("tempcorr_microbial"),
           reaction_rates=list(equations=list(R15="k12 * tempcorr_microbial * FeS * S0"), u_unit="mol/V_sf/y"),
           activated=TRUE),
  
  E23=list(abbreviation="E23",
           name="Conversion of Fe carb to FeS",
           involved_species=list(
             educts=list(
               "FeCO3"=list(abbreviation="FeCO3", stoic=1),
               "H2S"=list(abbreviation="H2S", stoic=1)),
             products=list(
               "FeS"=list(abbreviation="FeS", stoic=1),
               "DIC"=list(abbreviation="DIC", stoic=1))),
           reaction_rate_constants=list(k19=list(value=8e-4, u_unit="m3 mol-1 y-1")),
           shared_terms=list("tempcorr_microbial"),
           reaction_rates=list(equations=list(R25="k19 * tempcorr_microbial * FeCO3 * H2S"), u_unit="mol/V_sf/y"),
           activated=TRUE),
  
  E40=list(abbreviation="E40",
           name="Phosphate adsorption (Langmuir equilibrium regulated)",
           involved_species=list(
             educts=list(
               "PO4"=list(abbreviation="PO4", stoic=1)),
             products=list(
               "adsorbed_P"=list(abbreviation="adsorbed_P", stoic=1))),
           reaction_rate_constants=list(k_adsP=list(value=3650, u_unit="yr-1")),
           shared_terms=list("KadsP", "ads_t", "aP_e", "phosphate_load_FeOH3A"),
           reaction_rates=list(equations=list(R_adsP="k_adsP * (aP_e - adsorbed_P) * ifelse(aP_e > adsorbed_P, 1, 0)"), u_unit="mol/V_sf/y"),
           activated=FALSE),
  
  E41=list(abbreviation="E41",
           name="Phosphate desorption (Langmuir equilibrium regulated)",
           involved_species=list(
             educts=list(
               "adsorbed_P"=list(abbreviation="adsorbed_P", stoic=1)),
             products=list(
               "PO4"=list(abbreviation="PO4", stoic=1))),
           reaction_rate_constants=list(k_desP=list(value=3650, u_unit="yr-1")),
           shared_terms=list("KadsP", "ads_t", "aP_e", "phosphate_load_FeOH3A"),
           reaction_rates=list(equations=list(R_desP1="-1 * k_desP * (aP_e - adsorbed_P) * ifelse(aP_e < adsorbed_P, 1, 0)"), u_unit="mol/V_sf/y"),
           activated=FALSE),
  
  E42=list(abbreviation="E42",
           name="Phosphate desorption (release of adsorbed phosphate through chemical reactions)",
           involved_species=list(
             educts=list(
               "adsorbed_P"=list(abbreviation="adsorbed_P", stoic=1)),
             products=list(
               "PO4"=list(abbreviation="PO4", stoic=1))),
           reaction_rate_constants=list(), # none
           reaction_rates=list(equations=list(R_desP2="ifelse(RFeOH3A < 0, -1*RFeOH3A*phosphate_load_FeOH3A, 0)"), u_unit="mol/V_sf/y"),
           activated=FALSE)
    
)