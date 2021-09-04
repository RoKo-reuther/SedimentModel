
###########################################################################################################
#                                             MODEL FORMULATION                                           #
###########################################################################################################

Model <- function(t, state, pars) {
  
  ## Initialisation of state variables
  N <- parameters$N # get N out of parameters list
  for (i in seq_along(species_operational)){
    assign(names(species_operational[i]), state[((i-1)*N+1):(i*N)])
  }
  
  
  ## get current temperature
  TC <- t_functions$TC_func(t)

  
  ## Define the reaction terms
    # assign reaction rate constants
    for (i in seq_along(rate_constants)){
      assign(names(rate_constants[i]), rate_constants[[i]])
      }
    
    # assign shared regulation terms
    for (i in seq_along(shared_reg_terms)){
      assign(names(shared_reg_terms[i]), eval(parse(text=shared_reg_terms[[i]])))
      #print(names(shared_reg_terms[i]))
      #print(eval(parse(text=shared_reg_terms[[i]])))
    }

    # assign rate equations (except R_desP2)
    temp_list <- rate_equations[names(rate_equations) != "R_desP2"]
    for (i in seq_along(temp_list)){
      assign(names(temp_list[i]), eval(parse(text=temp_list[[i]]))) 
    }
  
    # assign reaction terms (except PO4 and adsorbed_P)
    temp_list <- reaction_terms[names(reaction_terms) != "Radsorbed_P" &  names(reaction_terms) != "RPO4"]
    for (i in seq_along(temp_list)){
      assign(names(temp_list[i]), eval(parse(text=temp_list[[i]]))) 
    }
    
    # assign R_desP2 (now RFeOH3A is known, which is needed to evaluate R_desP)
    assign("R_desP2", eval(parse(text=rate_equations["R_desP2"])))
    
    # assign RPO4 and Radsorbed_P (now R_desP2 is known, which is needed to evaluate those)
    temp_list <- reaction_terms[names(reaction_terms) == "Radsorbed_P" |  names(reaction_terms) == "RPO4"]
    for (i in seq_along(temp_list)){
      assign(names(temp_list[i]), eval(parse(text=temp_list[[i]]))) 
    }
    
    
  ## Define the transport terms
    # set temperature dependent diffusion coefficients for solutes
    grid_collection$solute_diffusion_coffs(TC)
    
    # Define boundary conditions
    # assign constant values
    for (i in seq_along(boundary_conditions$constant)){
      assign(names(boundary_conditions$constant[i]), boundary_conditions$constant[[i]])
    }
    # assign varying (time dependent) values
    for (i in seq_along(boundary_conditions$varying)){
      assign(names(boundary_conditions$varying[i]), boundary_conditions$varying[[i]](t))
    }
    
    # evaluete "transport_terms"
    for (i in seq_along(transport_terms)){
      assign(names(transport_terms[i]), eval(parse(text=transport_terms[[i]]))) 
    }
  
    
  ## Define "total concentration change terms"
  for (i in seq_along(total_c_change)){
    assign(names(total_c_change[i]), eval(parse(text=total_c_change[[i]]))) 
  }

      
  ## return statement
  return(eval(parse(text = returnlist)))
}