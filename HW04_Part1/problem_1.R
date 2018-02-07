## Define the hyperbolic tangent function

hyp_tan <- function(x) {
    
    ((exp(1)^x) - (exp(1)^-x))/((exp(1)^x) + (exp(1)^-x))
    
}

## Define the derivative of the hyperbolic tangent function

der_hyp_tan <- function(x) {
    
    (1 - (hyp_tan(x)^2))
    
}

## Initialize the network

num_layers <- 2
num_neurons <- c(2, 3, 1)
weights <- list()
biases <- list()

for (i in 1:num_layers) {
    
    weights[[i]] <- matrix(runif(num_neurons[i] * num_neurons[i + 1]), nrow = num_neurons[i + 1], ncol = num_neurons[i])
    biases[[i]] <- matrix(runif(num_neurons[i] * num_neurons[i + 1]), nrow = num_neurons[i + 1], ncol = 1)
    
}

## Initialize the training parameters