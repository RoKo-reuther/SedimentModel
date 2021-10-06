
###########################################################################################################
#                                   PLOT CONCENTRATION- AND RR-PROFILES                                   #
###########################################################################################################

library(ggplot2)
library(patchwork)

draw_depth_profile <- function(selection){
  #*************************************
  # depth vector
  #*************************************
  depth_p <- -1*grid_collection$grid$x.mid
  
  
  #*************************************
  # OMb-degradation
  #*************************************
  R1a <- data.frame(class="R1a", value=ss$R1a)
  RNa <- data.frame(class="RNa", value=ss$RNa)
  RMa <- data.frame(class="RMa", value=ss$RMa)
  R2a_Ox <- data.frame(class="R2a_Ox", value=ss$R2a_Ox)
  R3a <- data.frame(class="R3a", value=ss$R3a)
  R4a <- data.frame(class="R4a", value=ss$R4a)
  
  OMa <<- rbind(R1a, RNa, RMa, R2a_Ox, R3a, R4a)
  
  #*************************************
  # OMa-degradation
  #*************************************
  R1b <- data.frame(class="R1b", value=ss$R1b)
  RNb <- data.frame(class="RNb", value=ss$RNb)
  RMb <- data.frame(class="RMb", value=ss$RMb)
  R2b_Ox <- data.frame(class="R2b_Ox", value=ss$R2b_Ox)
  R3b <- data.frame(class="R3b", value=ss$R3b)
  R4b <- data.frame(class="R4b", value=ss$R4b)
  
  OMb <<- rbind(R1b, RNb, RMb, R2b_Ox, R3b, R4b)
  
  
  # #*************************************
  # # Mn2+
  # #*************************************
  # RMn_2 <- data.frame(class="RMn_2", value=ss$RMn_2)
  # RMa <- data.frame(class="RMa", value=ss$RMa)
  # RMb <- data.frame(class="RMb", value=ss$RMb)
  # R27 <- data.frame(class="R27", value=ss$R27)
  # R28 <- data.frame(class="R28", value=ss$R28)
  # R29_Ox <- data.frame(class="R29_Ox", value=ss$R29_Ox)
  # R29_P <- data.frame(class="R29_P", value=ss$R29_P)
  # R30 <- data.frame(class="R30", value=ss$R30)
  # R34 <- data.frame(class="R34", value=ss$R34)
  # 
  # Mn_2 <<- rbind(RMn_2, RMa, RMb, R27, R28, R29_Ox, R29_P, R30, R34)
  # 
  # 
  # #*************************************
  # # MnO2A
  # #*************************************
  # RMnO2A <- data.frame(class="RMnO2A", value=ss$RMnO2A)
  # RMa <- data.frame(class="RMa", value=ss$RMa)
  # RMb <- data.frame(class="RMb", value=ss$RMb)
  # R28 <- data.frame(class="R28", value=ss$R28)
  # R29_Ox <- data.frame(class="R29_Ox", value=ss$R29_Ox)
  # R29_P <- data.frame(class="R29_P", value=ss$R29_P)
  # R30 <- data.frame(class="R30", value=ss$R30)
  # R34 <- data.frame(class="R34", value=ss$R34)
  # 
  # MnO2A <<- rbind(RMnO2A, RMa, RMb, R28, R29_Ox, R29_P, R30, R34)
  # 
  # 
  # #*************************************
  # # FeOH3A
  # #*************************************
  # RFeOH3A <- data.frame(class="RFeOH3A", value=ss$RFeOH3A)
  # R2a_Ox <- data.frame(class="R2a_Ox", value=ss$R2a_Ox)
  # R2b_Ox <- data.frame(class="R2b_Ox", value=ss$R2b_Ox)
  # R5_Ox <- data.frame(class="R5_Ox", value=ss$R5_Ox)
  # R10_Ox <- data.frame(class="R10_Ox", value=ss$R10_Ox)
  # R29_Ox <- data.frame(class="R29_Ox", value=ss$R29_Ox)
  # 
  # FeOH3A <<- rbind(RFeOH3A, R2a_Ox, R2b_Ox, R5_Ox, R10_Ox, R29_Ox)
  
  
  #*************************************
  # draw profiles
  #*************************************
  draw_frame <- cbind(depth_p, selection)
  
  ggplot(data = draw_frame) + 
    geom_path(mapping = aes(x = value, y = depth_p, color = class))
}

draw_depth_profile(OM)


#*************************************
# concentration profiles
#*************************************
# draw concentration profiles for species in species-vector
concentration_profiles <- function(scenario_mode, species=NULL, draw_mode = "facet_wrap", ss_data=ss, trans_data=trans){
    # species: vector of species to draw concentration profile
    # scenario_mode: either ss (steady-state) or trans (transient)
    # draw_mode: either "facet_wrap", "collective"
  
  ## steady state scenario
  # create complete concentrations data-frame 
  df.ss_cs <- data.frame(
    depth = rep(-1*grid_collection$grid$x.mid, length(species_operational)),
    concentration = c(ss_data$y),
    specie = rep(attributes(ss_data$y)$dimnames[[2]], each=parameters$N))
  
  # correct for solid volume fraction/porosity -> get all conentrations in mol/m³ total volume
  for (element in species_operational){
    if (element$phase == "solute"){
      df.ss_cs[df.ss_cs$specie == element$name, "concentration"] <- df.ss_cs[df.ss_cs$specie == element$name, "concentration"] * grid_collection$por.grid$mid
    }
    else if (element$phase == "solid"){
      df.ss_cs[df.ss_cs$specie == element$name, "concentration"] <- df.ss_cs[df.ss_cs$specie == element$name, "concentration"] * grid_collection$svf.grid$mid
    }
  }
  
  # get requested subset data frame to plot (if species are specified)
  if (is.null(species)){
    df.plot <- df.ss_cs
  } else {
    df.plot <- df.ss_cs[df.ss_cs$specie %in% species, ]
  }
  
  # plot
  myplot <- ggplot(data = df.plot) +
    geom_path(mapping = aes(concentration, depth, color=specie)) +
    ggtitle("concentration profiles") +
    labs(x="concentration (mol/m³)", y = "depth (m)")
  
  if (draw_mode == "facet_wrap"){
    myplot <- myplot +
      scale_color_discrete(guide = "none") +
      facet_wrap(~specie, scales = "free_x", ncol = 3)
  }
  else if (draw_mode == "collective"){
    # nothing to change
  }
  myplot
}
concentration_profiles(scenario_mode = "ss", c("PO4", "adsorbed_P", "FeOH3A"), draw_mode = "collective")
concentration_profiles(scenario_mode = "ss", draw_mode = "facet_wrap")


#*************************************
# annual cycle
#*************************************
# draw annual cycle of varying parameters and boundary conditions
annual_cycle <- function(parms, mixis = c(TRUE, 0.67)){
  # parms: named list of parameters/boundary conditions to be drawn
  
  ## set timespan: one year by default
  times=seq(from=0, to=1, by=0.01)
  
  ## create plot for each parm
    # empty list to store plots
    plots <- list()
    
    for (i in seq_along(parms)){
      
      # set temp_name
      temp_name <- names(parms[i])
      
      # create data frame for parameter
      temp_df <- data.frame(
        time=times,
        value=parms[[i]](times) # function derived values in mol/m³
      )
      
    tempplot <- ggplot(data = temp_df, mapping = aes(time, value), color = "black") +
      theme_bw() +
      theme(plot.tag.position = "left", # customize theme
            plot.tag = element_text(face="bold", size=9, vjust = 15),
            panel.grid.major.x = element_line(size=0),
            plot.margin = margin(t=5, r=10, b=0, l=10),
            axis.title = element_text(size=8),
            axis.title.y.right = element_text(vjust = 2),
            title = element_text(family = "Helvetica-Narrow")) +
      labs(tag = temp_name) + # add parameter name
      scale_x_continuous(expand = c(0,0), limits = c(0, 1), # configure x-axis
                         name = NULL,
                         breaks = seq(0.5/12, 11.5/12, 1/12),
                         labels = c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D")) # x-scale
    
    # add data points from .csv file
    temp.data <- read.delim2(paste("./imports/", temp_name, ".csv", sep=""))
    tempplot <- tempplot +
      geom_point(data=temp.data, mapping=aes_string("time", paste(temp_name)), colour = "darkgrey")
    
    # add data line
    tempplot <- tempplot +
      geom_line(size = 0.8)
     
    # all parms except the last one: plot without x-axis and without "mixis-label"
    if (i < length(parms)){
      tempplot <- tempplot +
        theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.title.x = element_blank()) # no x-axis elements
    } else {
      tempplot <- tempplot +
        geom_text(mapping=aes(x = mixis[2], y=0, label="mixis"), colour="#666666", size=4, family = "", angle = 90, vjust=-0.4, hjust = -0.5, show.legend = FALSE) # "mixis"-label
      }
    
    # configure y-axis for different parms
    if (temp_name == "temperature"){ tempplot <- tempplot +
      labs(y="(°C)") # only name y-axis
    } else if (temp_name == "O2") {tempplot <- tempplot +
      scale_y_continuous(name="(mol/m³)", sec.axis = sec_axis(trans = ~.* 32, name = "(mg/l)")) # add second y-axis and name both
    } else if (temp_name == "NH4") {tempplot <- tempplot +
      scale_y_continuous(name="(mol/m³)", sec.axis = sec_axis(trans = ~.* 14, name = "(mg/l)")) # add second y-axis and name both
    } else if (temp_name == "NO3") {tempplot <- tempplot +
      scale_y_continuous(name="(mol/m³)", sec.axis = sec_axis(trans = ~.* 62, name = "(mg/l)")) # add second y-axis and name both
    }
    
    # add dashed mixis line if requested
    if (mixis[1] == TRUE){
      tempplot <- tempplot +
        geom_vline(xintercept = mixis[2], linetype = "dashed", colour="#666666")
      }
      
    # add plot for parm i to list
    plots[[i]] <- tempplot
    }
    
  return(plots)
}

plot <- annual_cycle(parms = list(
                temperature = parameters$TC_func,
                O2 = boundary_conditions$varying$O2_top,
                NH4 = boundary_conditions$varying$NH4_top,
                NO3 = boundary_conditions$varying$NO3_top
                ))

plot <- patchwork::wrap_plots(plot, ncol=1) +
        plot_annotation(theme = theme(title = element_text(family = "Helvetica-Narrow")),
                        title="annual cycle of varying parameters",
                        caption="Quelle der Daten: Bayerisches Landesamt für Umwelt, www.gkd.bayern.de")
plot
png(filename = "./exports/plots/annual_cycle.png", width = 560, height = 560, units = "px", pointsize = 2, res = 100)
plot
dev.off()
