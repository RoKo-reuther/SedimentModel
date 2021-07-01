
###########################################################################################################
#                                        CONFIGURE PARAMETERS HERE                                        #
###########################################################################################################

# Grid domain
  L <- 0.3    # depth of sediment domain [m]
  N <- 30    # number of grid layers

# Environmental parameters
  # constant porosity
  por             <- 0.8             # Porosity; for constant porosity
  # varying porosity (!!!not functional yet!!!)
  por.0         <- 0.8               # porosity at surface [-] =soil moisture conntent (at full saturation)
  por.inf       <- 0.8               # porosity at infinite depth [-]
  svf             <- 1-por           # Solid volume fraction
  TC              <- 10              # Temperature Celsius
  P               <- 0.30            # True pressure [bar] 
  dens_dw         <- 2.70            # Density dry sediment [g cm-3]= [kg L-1] = [10^3kg m-3] 
  S               <- 6.25* 10^-5     # Salinity for fresh water in psu (ppt)
  v               <- 10*1e-3         # Sedimentation rate: advection at top [m y-1]
  Db              <- 1e-4           #  Mixing rate of the sediment; Bioturbation coefficient [m2 y-1]
  
# stoichiometric relationships
  CtoN            <- 106/16          # C:N ratio after Redfield
  CtoP            <- 106/1           # C:P ratio after Redfield
  chi_FeOxA       <- 0.6             # P:Fe ratio; ratio adsorbed/coprecipitated P with Fe(OH)3... value of 0.6 by bas vd grift