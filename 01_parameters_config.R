
###########################################################################################################
#                                        CONFIGURE PARAMETERS HERE                                        #
###########################################################################################################

# Grid domain
  L <- 0.3    # depth of sediment domain [m]
  N <- 30   # number of grid layers

# Environmental parameters
  # porosity: set por.0 and por.inf equal for constant porosity
  por_shape     <- 1                # to shape porosity profile (used as x.att in p.exp (cf. "03_grid_setup.R"))
  por.0         <- 0.8              # porosity at surface [-] =soil moisture content (at full saturation)
  por.inf       <- 0.8              # porosity at infinite depth [-]
  
  P               <- 0.30           # True pressure [bar] 
  dens_dw         <- 2.70           # Density dry sediment [g cm-3]= [kg L-1] = [10^3kg m-3] ###where is it used?###
  S               <- 6.25* 10^-5    # Salinity for fresh water in psu (ppt); K: 430uS/cm -> 0.208 ppt?
  v               <- 10*1e-3        # Sedimentation rate: advection at top [m y-1]
  Db              <- 1e-4           # Mixing rate of the sediment; Bioturbation coefficient [m2 y-1]
  # temperature (potentially varying) [°C]
  TC_func <- function(t){
    TC <- 10 # set standard value; at least used for steady state calculation
    return(TC)
  }         
  
# stoichiometric relationships
  CtoN            <- 106/16          # C:N ratio after Redfield
  CtoP            <- 106/1           # C:P ratio after Redfield
  chi_FeOxA       <- 0.6             # P:Fe ratio; ratio adsorbed/coprecipitated P with Fe(OH)3... value of 0.6 by bas vd grift
  
# time parameters
  #Time and time steps for solving transient model after steady state solution has been found
  tmax <- 20    # number of years for run
  tint <- 1/12  # time step [years]
  
  #create time sequence
  times <- seq(0, tmax, by = tint)