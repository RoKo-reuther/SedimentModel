
###########################################################################################################
#                           DONT CHANGE ANYTHING HERE WITHOUT INTENDED PURPOSE                            #
###########################################################################################################

library(stringr)

#*************************************
# define functions
#*************************************

get_boundary_conditions <- function(){
  # assign "boundary_conditions"-list from "boundary_conditions_config" to globalENV
  t = 0 # minor temporary tweak to source this file without error
  source(file=configs$boundary_conditions_config, local = TRUE)
  boundary_conditions <<- boundary_conditions
}

get_times_sequence <- function(){
   # assign "times" from "parameters_config" to globalENV: needed to fill in X_Add and X_Trans values, and for transient run
  source(file=configs$parameters_config, local=TRUE)
  times <<- times
}

get_factors <- function(t){
  # basically "transient-function": returns X_Add and X_Trans for a timestep for all species in a vector
  
  # set standards and create return list
    # prepare return statement
    temp_return <- ""
    
    # set X_Add standard to 0
    for (species in names_out){
        # variable name
        var_name <- paste(species, "_Add", sep = "")
        # assign variable
        assign(var_name, 0)
        # add to return-vector
        temp_return <- paste(temp_return, var_name, sep = ", ")
    }
    
    # set X_Trans standard to 1
    for (species in names_out){
        # variable name
        var_name <- paste(species, "_Trans", sep = "")
        # assign variable
        assign(var_name, 1)
        # add to return-vector
        temp_return <- paste(temp_return, var_name, sep = ", ")
    }
  
  # load time-configured boundary-conditions
    # round timestep for expected behaviour
    t <- round(t, digits=6)
  source(file=configs$boundary_conditions_config, local = TRUE)
  
  # return vector(X_Add..., X_Trans...)
  temp_return <- paste("c(", str_sub(temp_return, start=3), ")", sep = "")
  return(eval(parse(text = temp_return)))
}


#*************************************
# call functions
#*************************************

# get boundary_conditions list
get_boundary_conditions()

# get times-sequence
get_times_sequence()

# create boundary condition-factor array: X_Add and X_Trans for every species and timestep
boundary_condition_factors <- array(NA, dim=c(length(times), length(names_out), 2), dimnames=list(times, names_out, c("X_Add", "X_Trans")))

# fill in X_Add and X_Trans values
for (i in seq_along(times)){
  boundary_condition_factors[i,,] <- get_factors(times[i])
}

# clean up
rm(get_boundary_conditions, get_times_sequence, get_factors, i)
