## Define the hyperbolic tangent function

hyp_tan <- function(x) {
    
    ((exp(1)^x) - (exp(1)^-x))/((exp(1)^x) + (exp(1)^-x))
    
}

## Define the derivative of the hyperbolic tangent function

der_hyp_tan <- function(x) {
    
    (1 - (hyp_tan(x)^2))
    
}

