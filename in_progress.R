for (i in seq(from=1, to=parameters$N)){
  print(trans[ , paste("SI_viv", i, sep="")])
}

### OMEGA forcement
p1 <- 36787.94 # where does alternative start
p2 <- 1e-5 # prevents rate from getting zero at omega=1

# function to prevent apruptly zero values
test <- function(SI_FeCO3){
  1/(p1*exp(abs(SI_FeCO3-1)+1))
}
curve(test, from=0, to = 2)

dissolution <- function(SI_FeCO3){
  ifelse(SI_FeCO3 <= 1, SI_FeCO3-1+p2, 1/(p1*exp(abs(SI_FeCO3-1)+1)))
}
curve(dissolution, from=0.9999, to = 1.0001)

precipitation <- function(SI_FeCO3){
  ifelse(SI_FeCO3 > 1, SI_FeCO3-1+p2, 1/(p1*exp(abs(SI_FeCO3-1)+1)))
}
curve(precipitation, from=0.9999, to = 1.0001)


myfunc <- function(p1){
  p2 <- 1e-5 # prevents rate from getting zero at omega=1
  dissolution_a <- function(p1, SI_FeCO3=1){
    ifelse(SI_FeCO3 <= 1, SI_FeCO3-1+p2, 1/(p1*exp(abs(SI_FeCO3-1)+1)))
  }
  
  dissolution_b <- function(p1, SI_FeCO3=1){
    ifelse(SI_FeCO3 < 1, SI_FeCO3-1+p2, 1/(p1*exp(abs(SI_FeCO3-1)+1)))
  }
  
  abs(dissolution_a(p1) - dissolution_b(p1))
}

optimize(f=myfunc, interval=c(1e4, 1e6))


## notes
precip_rate_FeCO3="k18*Fe_2*DIC*ifelse(SI_FeCO3 > 1, SI_FeCO3-1+1e-15, 1/(3.678794e+14*exp(abs(SI_FeCO3-1)+1)))"
diss_rate_FeCO3="-k32*FeCO3*ifelse(SI_FeCO3 <= 1, SI_FeCO3-1+1e-15, 1/(3.678794e+14*exp(abs(SI_FeCO3-1)+1)))"

precip_rate_viv="k20*Fe_2*PO4*ifelse(SI_viv > 1, SI_viv-1+1e-15, 1/(3.678794e+14*exp(abs(SI_viv-1)+1)))"
diss_rate_viv="-k21*VivP*ifelse(SI_viv <= 1, SI_viv-1+1e-15, 1/(3.678794e+14*exp(abs(SI_viv-1)+1)))"

plot.1D(trans, xyswap = TRUE, select = c("precip_rate_FeCO3", "diss_rate_FeCO3", "SI_FeCO3", "Fe_2", "DIC", "FeCO3"), ask=TRUE)


###################################################
# tweaked tran.1D term for adsorbed_P
tran.1D(C=FeOH3A*phosphate_load_FeOH3A, flux.up=ifelse(tranFeOH3A$flux.up<0, tranFeOH3A$flux.up*phosphate_load_FeOH3A, 0), flux.down=ifelse(tranFeOH3A$flux.down>0, tranFeOH3A$flux.down*phosphate_load_FeOH3A, 0), D=grid_collection$Db.grid, v=grid_collection$v.grid, VF=grid_collection$svf.grid, dx=grid_collection$grid, full.output =TRUE)
# rate equations without R_desP2
rate_equations[names(rate_equations) != "R_desP2"]
# RX terms without PO4 and adsorbed_P
reaction_terms[names(reaction_terms) != "Radsorbed_P" &  names(reaction_terms) != "RPO4"]
# only R_desP2
rate_equations["R_desP2"]
# only RX for adsorbed_P and PO4
reaction_terms[names(reaction_terms) == "Radsorbed_P" |  names(reaction_terms) == "RPO4"]
