 
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
