
###########################################################################################################
#                                   PLOT CONCENTRATION- AND RR-PROFILES                                   #
###########################################################################################################

library(ggplot2)
library(patchwork)


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
