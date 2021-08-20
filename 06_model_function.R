
###########################################################################################################
#                                             MODEL FORMULATION                                           #
###########################################################################################################

Model <- function(t, state, pars) {
  
  ## Initialisation of state variables
  N <- parameters$N # get N out of parameters list
  for (i in seq_along(species_operational)){
    assign(names(species_operational[i]), state[((i-1)*N+1):(i*N)])
  }


  ## Define boundary conditions
    # assign constant values
    for (i in seq_along(boundary_conditions$constant)){
      assign(names(boundary_conditions$constant[i]), boundary_conditions$constant[[i]])
    }
    # assign varying (time depemdent) values
  for (i in seq_along(boundary_conditions$varying)){
    assign(names(boundary_conditions$varying[i]), boundary_conditions$varying[[i]](t))
  }

  
  ## get current temperature
  TC <- TC_func(t)
  
  
  ## set temperature diffusion coefficients for solutes
  grid_collection$diff_calculations$solute_diffusion_coffs(TC)
  
  
  ## Define the transport terms
  for (i in seq_along(transport_terms)){
    assign(names(transport_terms[i]), eval(parse(text=transport_terms[[i]]))) 
  }
  
  
  ## Define the reaction terms
    # assign reaction rate constants
    for (i in seq_along(rate_constants)){
      assign(names(rate_constants[i]), rate_constants[[i]])
      }
    
    # assign shared regulation terms
    for (i in seq_along(shared_reg_terms)){
      assign(names(shared_reg_terms[i]), eval(parse(text=shared_reg_terms[[i]])))
    }
    
    # assign rate equations
    for (i in seq_along(rate_equations)){
      assign(names(rate_equations[i]), eval(parse(text=rate_equations[[i]]))) 
    }
  
    # assign reaction terms
    for (i in seq_along(reaction_terms)){
      assign(names(reaction_terms[i]), eval(parse(text=reaction_terms[[i]]))) 
    }
  
    
  ## Define "total concentration change terms"
  for (i in seq_along(total_c_change)){
    assign(names(total_c_change[i]), eval(parse(text=total_c_change[[i]]))) 
  }
  
  
  ## return statement
  return(eval(parse(text = returnlist)))
}