
library(FME)
#library(calibrar)
library(ggplot2)
library(patchwork)

#*************************************
# fit porosity
#*************************************

myfunc <- function(parms) {
  por.0 <- parms[1]
  por.inf <- parms[2]
  por_shape <- parms[3]
  
  testpor <<- setup.prop.1D(func=p.exp, grid=grid_collection$grid, y.0 = por.0, y.inf = por.inf, x.att = por_shape)
  fun_por <<- approxfun(grid_collection$grid$x.mid, testpor$mid)
  sum((fun_por(porosity[["depth"]]) - porosity[["porosity"]])**2)
}
porosity <- read.delim2("~/Dokumente/SedimentModel/imports/porosity.csv")
porosity <- porosity[-2,] # does apparently not fit to curve
optim(par = c(0.9574446, 0.9020287, 0.1225658), myfunc)

plot_porosity <- function() {
  df_modeled <- data.frame(
    depth = grid_collection$grid$x.mid,
    porosity = testpor$mid,
    type = "modeled"
  )
  df_measured <- cbind(porosity,
                     type = "measured")
  df_porosity <- rbind(df_modeled, df_measured)
  ggplot(data=df_porosity, mapping = aes(x = porosity, y=depth, color=type)) +
    geom_line() + 
    geom_point() +
    scale_y_reverse()
}
plot_por <- plot_porosity()
plot_por

#*************************************
# fit sediment density
#*************************************

myfunc <- function(parms) {
  sed_dens.0 <- parms[1]
  sed_dens.inf <- parms[2]
  sed_dens_shape <- parms[3]
  
  testsed_dens <<- setup.prop.1D(func=p.exp, grid=grid_collection$grid, y.0 = sed_dens.0, y.inf = sed_dens.inf, x.att = sed_dens_shape)
  fun_sed_dens <<- approxfun(grid_collection$grid$x.mid, testsed_dens$mid)
  sum((fun_sed_dens(sediment_density[["depth"]]) - sediment_density[["sediment_density"]])**2)
}
sediment_density <- read.delim2("~/Dokumente/SedimentModel/imports/sediment_density.csv")
optim(par = c(2.25041639, 2.33802349, 0.09882358), myfunc)

plot_sediment_density <- function() {
  df_modeled <- data.frame(
    depth = grid_collection$grid$x.mid,
    sediment_density = testsed_dens$mid,
    type = "modeled"
  )
  df_measured <- cbind(sediment_density,
                       type = "measured")
  df_sediment_density <- rbind(df_modeled, df_measured)
  ggplot(data=df_sediment_density, mapping = aes(x = sediment_density, y=depth, color=type)) +
    geom_path() + 
    geom_point() +
    scale_y_reverse()
}
plot_sed_dens <- plot_sediment_density()
plot_sed_dens

#*************************************
# fit model to measured concentrations
#*************************************

## define fitting function
myfunc <- function(parms, modus, mymethod){
  # modus: either "fitting", "sensitivity" or "plotting"; determines the return statement
  
  print(parms)
    
    for (i in parms_coll$targets){
      try(eval(parse(text = i)), silent = TRUE)
    }
  
  # model run with actual parameter setting
  # try block: catch model fails and return appropriate results for different modi
  result <- tryCatch({
    print(system.time(
      ss_temp <<- steady.1D(y = state,
                           time = 0.35,
                           func = Model, 
                           parms = NULL, 
                           names = names_out, 
                           method = "stodes",
                           #verbose=TRUE,
                           pos = TRUE,
                           nspec = length(names_out) #,rtol = 1e-16,ctol = 1e-16 ,atol = 1e-16
      )))
    
    if ("OM_d" %in% names(data_list)){
      ## CALCULATION OF %OM
      # ASSUMPTIONS:
      #   LOI ~ %OM (respectively reffered to solid fraction)
      #   weight of one mole OM in this model: 33.64 g/mol
      OrgC_tracer <- attributes(ss_temp$y)$dimnames[[2]] %in% c("OrgCA", "OrgCB", "OrgCC")
      total_OrgC <- apply(ss_temp$y[, OrgC_tracer], c(1), sum) # mol/m³_sf
      total_OrgC <- total_OrgC * 33.64151 # g/m³_sf
      perc_OM <- total_OrgC / (grid_collection$sed_dens.grid$mid*1e6) * 100
    }
    
    # result processing and return statements for different modi
    if (modus == "sensitivity"){
      # return result matrix for sensitivity analysis
      return(ss_temp$y)
    }
    else if (modus == "plotting"){
      # build data frames: temp_df1: measured data; temp_df2: model derived data; temp_df: combined
      temp_df <- data.frame()
      for (i in data_list){
        if (names(i)[2] != "OM"){
          temp_df1 <- data.frame(depth = i[[1]], value = i[[2]], type = "measured", specie = names(i)[2])
          temp_df2 <- data.frame(depth = grid_collection$grid$x.mid, value = ss_temp$y[, specie = names(i)[2]], type = "model derived", specie = names(i)[2])
          temp_df <- rbind(temp_df, temp_df1, temp_df2)
        }
        else if (names(i)[2] == "OM"){
          temp_df1 <- data.frame(depth = i[[1]], value = i[[2]], type = "measured", specie = names(i)[2])
          temp_df2 <- data.frame(depth = grid_collection$grid$x.mid, value = perc_OM, type = "model derived", specie = names(i)[2])
          temp_df <- rbind(temp_df, temp_df1, temp_df2)
        }
      }
      
      # create plots for actual parms
      # a) solids
      solids <- temp_df[, "specie"] %in% c("FeOH3A", "FeS2", "OM")
      temp_plot1 <- ggplot(data = temp_df[temp_df["type"]=="model derived" & solids, ]) +
        geom_path(aes(value, -depth, colour=specie)) +
        geom_point(data=temp_df[temp_df["type"]=="measured" & solids, ], aes(value, -depth, colour=specie))
      
      # b) solutes
      solutes <- ! solids
      temp_plot2 <- ggplot(data = temp_df[temp_df["type"]=="model derived" & solutes, ]) +
        geom_path(aes(value, -depth, colour=specie)) +
        geom_point(data=temp_df[temp_df["type"]=="measured" & solutes, ], aes(value, -depth, colour=specie))
      
      # store plot in plot-list
      temp_plot <- patchwork::wrap_plots(temp_plot1, temp_plot2)
      
      # return plot
      return(temp_plot)
    }
    else if (modus == "fitting"){
      # build functions to calculate differences (residuals)
      fun_FeOH3A <- approxfun(grid_collection$grid$x.mid, ss_temp$y[, "FeOH3A"])
      fun_FeS2 <- approxfun(grid_collection$grid$x.mid, ss_temp$y[, "FeS2"])
      fun_Fe_2 <- approxfun(grid_collection$grid$x.mid, ss_temp$y[, "Fe_2"])
      fun_NO3 <- approxfun(grid_collection$grid$x.mid, ss_temp$y[, "NO3"])
      fun_H2S <- approxfun(grid_collection$grid$x.mid, ss_temp$y[, "H2S"])
      fun_SO4 <- approxfun(grid_collection$grid$x.mid, ss_temp$y[, "SO4"])
      fun_O2 <- approxfun(grid_collection$grid$x.mid, ss_temp$y[, "O2"])
      fun_OM <- approxfun(grid_collection$grid$x.mid, perc_OM)
      
      # expression to be minimized
      myresiduals <<- c(#fun_FeOH3A(data_list$FeOH3A_d[["depth"]])- data_list$FeOH3A_d[["FeOH3A"]],
                        #fun_FeS2(data_list$FeS2_d[["depth"]]) - data_list$FeS2_d[["FeS2"]],
                        #fun_Fe_2(data_list$Fe_2_d[["depth"]]) - data_list$Fe_2_d[["Fe_2"]],
                        (fun_NO3(data_list$NO3_d[["depth"]]) - data_list$NO3_d[["NO3"]]),
                        #fun_H2S(data_list$H2S_d[["depth"]]) - data_list$H2S_d[["H2S"]],
                        #fun_SO4(data_list$SO4_d[["depth"]]) - data_list$SO4_d[["SO4"]],
                        (fun_O2(data_list$O2_d[["depth"]]) - data_list$O2_d[["O2"]]),
                        fun_OM(data_list$OM_d[["depth"]]) - data_list$OM_d[["OM"]]
      )
      
      ssr <<- sum(myresiduals**2)
      
      # return residuals / sum of squared residuals for fitting
      if (mymethod %in% c("Pseudo", "Brent", "CG", "AHR-ES", "L-BFGS-B", "Nelder-Mead")){
        return(ssr)
      }
      else {
        return(myresiduals) 
      }
    }
  },
  error = function(e){
    
    # result processing and return statements for different modi in case of model failing
    if (modus == "sensitivity"){
      # return result matrix for sensitivity analysis
      return(ss_temp$y)
    }
    else if (modus == "plotting"){
      print("an error occured")
      return()
    }
    else if (modus == "fitting"){
      # return residuals / sum of squared residuals for fitting
      if (mymethod %in% c("Pseudo", "Brent", "CG", "Nelder-Mead")){
        return(NA)
      }
      else if (mymethod %in% c("L-BFGS-B", "AHR-ES")){
        return(ssr)
      }
      else {
        return(myresiduals) 
      }
    }
  })
  # return result of try block
  return(result)
}

## preparations
  preps <- list(
    # # remove varying boundary cond. for FeOH3A
    # boundary_conditions$varying$F_FeOH3A <- NULL,
    # # set FeOH3A flux
    # boundary_conditions$constant$F_FeOH3A <- 0.2675634,
    # # set MnO2 flux
    # boundary_conditions$constant$F_MnO2A <- 3,
    # # set OrgC flux
    # boundary_conditions$constant$F_OrgCA  <- 3.362136 * 0.76,
    # boundary_conditions$constant$F_OrgCB  <- 3.362136 * 0.24,
    # prepare grid for O2 and H2S data ("high-resolution" at SWI)
    # implement F_OrgCC
    boundary_conditions$constant$F_OrgCC <- 0
  )

  # load in measured data; also controls plotted species
  data_list <- list(
    #FeOH3A_d = read.delim2("~/Dokumente/SedimentModel/imports/FeOH3A_d.csv"),
    #FeS2_d = read.delim2("~/Dokumente/SedimentModel/imports/FeS2_d.csv"),
    #Fe_2_d = read.delim2("~/Dokumente/SedimentModel/imports/Fe_2_d.csv"),
    NO3_d = read.delim2("~/Dokumente/SedimentModel/imports/NO3_d.csv"),
    #H2S_d = read.delim2("~/Dokumente/SedimentModel/imports/H2S_d.csv"),
    #SO4_d = read.delim2("~/Dokumente/SedimentModel/imports/SO4_d.csv"),
    O2_d = read.delim2("~/Dokumente/SedimentModel/imports/O2_d.csv"),
    OM_d = read.delim2("~/Dokumente/SedimentModel/imports/OM_d.csv") # w%OM of sediment weight
  )
  
  # collection of all parameters
  parms_coll <- list(
    # parms: initial values
    parms = c(Db           = 1.7e-3, # bioturbation coefficient
              Db_depth     = 0.1,
              F_OrgC_tot   = 7.4,     # total OM influx: F_OrgCA + F_OrgCB + F_OrgCC
              k_alpha      = 0.8,
              k_beta       = 0.0025
              #K_mO2        = 1e-3,
              #K_mNO3       = 4e-3,
              #K_mMnO2      = 200e-3,
              #K_mFeOH3     = 200e-3,
              #K_mSO4       = 1.6e-3,
              #k1           = 5e3, # Nitrifikation
              #k2           = 1.4e2, # FeOH3 formation
              #k5           = 1.6e2, # H2S oxidation
              #k6           = 1e8, # aerobic methane oxidation
              #k7           = 8, # FeOxA reduction coupled to sulphide oxidation
              #k9           = 0.1482, # FeS formation
              #k13          = 10, # SO4 reduction coupled to AOM
              #k23          = 0.265, # MnCO3 precipitation
              #k24          = 1, # Mn oxidation
              #k25          = 23.652, # MnO2A reduction coupled to Fe oxidation
              #k27          = 4e4, # Mn oxidation
              #k30          = 1.7e-3, # MnO2A reduction coupled to S oxidation
              #k18          = 1.89e-2, # FeCO3 formation with CO32-
              #k32          = 1.89e-2, # FeCO3 dissolution
              #k20          = 1.15e-1, # vivianite formation
              #k21          = 1.15e-1, # vivianite dissolution
              #k22          = 8e-4, # conversion of vivianite to FeS
              #k3           = 6e1, # FeS dissolution
              #k4           = 5e3, # FeS2 dissolution
              #k10          = 30, # pyrite formation with H2S
              #k11          = 1.6e2, # S0 transformation into SO4 and H2S
              #k12          = 7.26e3, # pyrite formation with S0
              #k19          = 8e-4, # conversion of FeCO3 to FeS
              #F_MnO2A      = 0.4527190,
              #F_FeOH3A     = 0.1640076
    ),
    
    # parms: lower limit
    lower = c(Db           = 4e-4, # bioturbation coefficient
              Db_depth     = 0.05,
              F_OrgC_tot   = 2.5,
              k_alpha      = 0.1,
              k_beta       = 0.0025
              #K_mO2        = 1e-3,
              #K_mNO3       = 1e-3,
              #K_mMnO2      = 1e-3,
              #K_mFeOH3     = 1e-3,
              #K_mSO4       = 1e-3,
              #k1           = 5e3,
              #k2           = 1.4e2, # FeOH3 formation
              #k5           = 1.6e2, # H2S oxidation
              #k6           = 1e8, # aerobic methane oxidation
              #k7           = 8, # FeOxA reduction coupled to sulphide oxidation
              #k9           = 0.1482, # FeS formation
              #k13          = 10, # SO4 reduction coupled to AOM
              #k23          = 0.265, # MnCO3 precipitation
              #k24          = 1, # Mn oxidation
              #k25          = 23.652, # MnO2A reduction coupled to Fe oxidation
              #k27          = 4e4, # Mn oxidation
              #k30          = 1.7e-3, # MnO2A reduction coupled to S oxidation
              #k18          = 1.89e-2, # FeCO3 formation with CO32-
              #k32          = 1.89e-2, # FeCO3 dissolution
              #k20          = 1.15e-1, # vivianite formation
              #k21          = 1.15e-1, # vivianite dissolution
              #k22          = 8e-4, # conversion of vivianite to FeS
              #k3           = 6e1, # FeS dissolution
              #k4           = 5e3, # FeS2 dissolution
              #k10          = 30, # pyrite formation with H2S
              #k11          = 1.6e2, # S0 transformation into SO4 and H2S
              #k12          = 7.26e3, # pyrite formation with S0
              #k19          = 8e-4, # conversion of FeCO3 to FeS
              #F_MnO2A      = 0.4527190,
              #F_FeOH3A     = 0.1640076
    ),
    
    # parms: upper limit
    upper = c(Db           = 4e-3, # bioturbation coefficient
              Db_depth     = 0.15,
              F_OrgC_tot   = 13.33,
              k_alpha      = 1.62,
              k_beta       = 0.0086
              #K_mO2        = 2e-1,
              #K_mNO3       = 2e-1,
              #K_mMnO2      = 2e-1,
              #K_mFeOH3     = 2e-1,
              #K_mSO4       = 2e-1,
              #k1           = 20e4,
              #k2           = 1.4e2, # FeOH3 formation
              #k5           = 1.6e2, # H2S oxidation
              #k6           = 1e8, # aerobic methane oxidation
              #k7           = 8, # FeOxA reduction coupled to sulphide oxidation
              #k9           = 0.1482, # FeS formation
              #k13          = 10, # SO4 reduction coupled to AOM
              #k23          = 0.265, # MnCO3 precipitation
              #k24          = 1, # Mn oxidation
              #k25          = 23.652, # MnO2A reduction coupled to Fe oxidation
              #k27          = 4e4, # Mn oxidation
              #k30          = 1.7e-3, # MnO2A reduction coupled to S oxidation
              #k18          = 1.89e-2, # FeCO3 formation with CO32-
              #k32          = 1.89e-2, # FeCO3 dissolution
              #k20          = 1.15e-1, # vivianite formation
              #k21          = 1.15e-1, # vivianite dissolution
              #k22          = 8e-4, # conversion of vivianite to FeS
              #k3           = 6e1, # FeS dissolution
              #k4           = 5e3, # FeS2 dissolution
              #k10          = 30, # pyrite formation with H2S
              #k11          = 1.6e2, # S0 transformation into SO4 and H2S
              #k12          = 7.26e3, # pyrite formation with S0
              #k19          = 8e-4, # conversion of FeCO3 to FeS
              #F_MnO2A      = 0.4527190,
              #F_FeOH3A     = 0.1640076
    ),
    
    # targets: define how parameters should influence our model environment
    targets = c("boundary_conditions$constant$F_OrgCA  <<- parms[['F_OrgC_tot']] * 0.50",
                "boundary_conditions$constant$F_OrgCB  <<- parms[['F_OrgC_tot']] * 0.16",
                "boundary_conditions$constant$F_OrgCC  <<- parms[['F_OrgC_tot']] * 0.34",
                "boundary_conditions$constant$F_MnO2A  <<- parms[['F_MnO2A']]",
                "boundary_conditions$constant$F_FeOH3A <<- parms[['F_FeOH3A']]",
                "boundary_conditions$constant$F_FeCO3  <<- parms[['F_FeCO3']]",
                "boundary_conditions$constant$F_MnCO3  <<- parms[['F_MnCO3]]",
                
                "rate_constants$k_alpha <<- parms[['k_alpha']]",
                "rate_constants$k_beta <<- parms[['k_beta']]",
                "rate_constants$K_mO2 <<- parms[['K_mO2']]",
                "rate_constants$K_mNO3 <<- parms[['K_mNO3']]",
                "rate_constants$K_mMnO2 <<- parms[['K_mMnO2']]",
                "rate_constants$K_mFeOH3 <<- parms[['K_mFeOH3']]",
                "rate_constants$K_mSO4 <<- parms[['K_mSO4']]",
                "rate_constants$k1 <<- parms[['k1']]",
                
                "grid_collection$Db.grid <<- setup.prop.1D(func=p.sig, grid = grid_collection$grid, y.0 = parms[['Db']], y.inf = 0,x.L = parms[['Db_depth']], x.att = 0.05)"
    )
  )
  
  # selection of parameters to fit
    # # either all listet parms ...
    # parms_sel <- seq_along(parms)
    # ... or selection by name
    parms_sel <- c("F_OrgC_tot", "k_alpha", "k_beta", "Db", "Db_depth")
  
## sensitivity analysis
  sF <- sensFun(myfunc, parms = parms_coll$parms[parms_sel], modus = "sensitivity")
  
## fitting
  # use "optimize" for singel parameter fitting (optim with method "Brent)
  print(system.time(fitvals_0 <- optim(par = parms_coll$parms[parms_sel], fn = myfunc, method = "Brent", lower = parms_coll$lower[parms_sel], upper = parms_coll$upper[parms_sel], modus = "fitting", mymethod = "Brent")))
  # use method "Pseudo" to get a first valid guess
  print(system.time(fitvals_1 <- pseudoOptim_mod(f = myfunc, p = parms_coll$parms[parms_sel], lower = parms_coll$lower[parms_sel], upper = parms_coll$upper[parms_sel], control = c(numiter = 4000), modus = "fitting", mymethod = "Pseudo")))
  # use method "Marq" (the default)
  print(system.time(fitvals_2 <- modFit(f = myfunc, p = parms_coll$parms[parms_sel], lower = parms_coll$lower[parms_sel], upper = parms_coll$upper[parms_sel], method = "Marq", modus = "fitting", mymethod = "Marq")))
  # use method "CG"
  print(system.time(fitvals_3 <- optim(par = parms_coll$parms[parms_sel], fn = myfunc, method = "CG", modus = "fitting", mymethod = "CG")))
  # use method "Nelder-Mead" (the default)
  print(system.time(fitvals_4 <- optim(par = parms_coll$parms[parms_sel], fn = myfunc, method = "Nelder-Mead", modus = "fitting", mymethod = "Nelder-Mead")))
  
## plot fitted curves
  plot_fitted_0 <- myfunc(parms = fitvals_0$par, modus = "plotting")
  plot_fitted_0
  plot_fitted_1 <- myfunc(parms = fitvals_1$par, modus = "plotting")
  plot_fitted_1
  plot_fitted_2 <- myfunc(parms = fitvals_2$par, modus = "plotting")
  plot_fitted_2
  plot_fitted_3 <- myfunc(parms = fitvals_3$par, modus = "plotting")
  plot_fitted_3
  plot_fitted_4 <- myfunc(parms = fitvals_4$par, modus = "plotting")
  plot_fitted_4
  plot_fitted_FME <- myfunc(parms = PFit$par, modus = "plotting")
  plot_fitted_FME
  
## plot manual parms selection
  manparms <- c("F_OrgC_tot" = 13.330000000, "k_alpha" = 1.62, "k_beta" = 0.005451214, "Db" = 0.003385050, "Db_depth" = 0.091279444)
  plot_fitted_man <- myfunc(parms = manparms, modus = "plotting")
  title <- ""
  for (i in seq_along(manparms)){title <- paste(title, paste(names(manparms[i]), manparms[i], sep = ": "), sep = "\n")}
  plot_fitted_man  + plot_annotation(subtitle=title)

#  temp
  ss <- ss_temp
  draw_depth_profile(OMa) + draw_depth_profile(OMb)
  
#****************************************************
# step 1: fit %OM
#****************************************************
