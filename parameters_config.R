
###########################################################################################################
#                                        CONFIGURE PARAMETERS HERE                                        #
###########################################################################################################


## Grid domain
L <- 0.3    # depth of sediment domain [m]
N <- 30   # number of grid layers

## Environmental parameters
# porosity: set por.0 and por.inf equal for constant porosity
por_shape     <- 0.1225706        # to shape porosity profile (used as x.att in p.exp (cf. "03_grid_setup.R"))
por.0         <- 0.95             # porosity at surface [-] =soil moisture content (at full saturation)
por.inf       <- 0.90             # porosity at infinite depth [-]

P               <- 0.30           # True pressure [bar]
dens_dw         <- 2.70           # Density dry sediment [g cm-3]= [kg L-1] = [10^3kg m-3]; used for K_mMnO2 and K_mFeOH3
S               <- 0.2            # Salinity for fresh water in psu (ppt); K: 430uS/cm -> 0.208 ppt?
v               <- 10*1e-3        # Sedimentation rate: advection at top [m y-1]
Db              <- 1e-4           # Mixing rate of the sediment; Bioturbation coefficient [m2 y-1]
Db_depth        <- 0.175          # middle of Db decreasing zone
# temperature (potentially varying) [°C]
# load in temperature one-year time series data
temp.data <- read.delim2("./imports/temperature.csv")

## stoichiometric relationships
CtoN            <- 106/16          # C:N ratio after Redfield
CtoP            <- 106/1           # C:P ratio after Redfield
#chi_FeOxA       <- 0.6             # P:Fe ratio; ratio adsorbed/coprecipitated P with Fe(OH)3... value of 0.6 by bas vd grift

## time parameters
#Time and time steps for solving transient model after steady state solution has been found
tmax <- 2    # number of years for run
tint <- 1/12  # time step [years]