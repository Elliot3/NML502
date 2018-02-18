## Load in necessary packages

library(ggplot2)

## Load in necessary functions

source("~/Documents/Rice_University/Spring_2018/NML502/HW04_Part1/forward_pass.R")
source("~/Documents/Rice_University/Spring_2018/NML502/HW04_Part1/back_pass.R")
source("~/Documents/Rice_University/Spring_2018/NML502/HW04_Part1/rms_error.R")

## Define the hyperbolic tangent function

trans_func <- function(x) {
    
    ((exp(1)^x) - (exp(1)^-x))/((exp(1)^x) + (exp(1)^-x))
    
}

## Define the derivative of the hyperbolic tangent function

der_trans_func <- function(x) {
    
    (1 - (trans_func(x)^2))
    
}

## Create a container for the errors and learning steps

errors <- numeric()
ler_step <- numeric()
input_vecs <- list()
expected_outputs <- numeric()
actual_outputs <- numeric()


## Initialize the network

num_layers <- 2
num_outputs <- c(2, 3, 1)

weights <- list()
biases <- list()

for (i in 1:num_layers) {
    
    weights[[i]] <- matrix(runif(num_outputs[i] * num_outputs[i + 1]), nrow = num_outputs[i + 1], ncol = num_outputs[i])
    biases[[i]] <- matrix(runif(num_outputs[i] * num_outputs[i + 1]), nrow = num_outputs[i + 1], ncol = 1)
    
}

## Initialize the training parameters

n <- 6250
ler_rate <- 0.05

## Import the training data

x <- matrix(c(1,1,0,0,1,0,1,0), nrow = 2, ncol = 4, byrow = T)
y <- matrix(c(0,1,1,0), nrow = 1, ncol = 4)
bias <- matrix(rep(1, 4), nrow = 1, ncol = 4)

## Iteratively learn the weights

for (i in 1:n) {
    
    ## Randomly permute the indices
    
    inds <- sample(1:dim(x)[2])
    
    ## Perform the permutation
    
    x <- x[, inds]
    y <- matrix(y[, inds], nrow = 1, ncol = dim(y)[2])
    
    ## Perform the back propagation for each pattern
    
    for (j in 1:dim(x)[2]) {
        
        ## Select the jth input pattern
        
        x_pat <- matrix(x[, j], nrow = dim(x)[1], ncol = 1)
        y_pat <- matrix(y[, j], nrow = dim(y)[1], ncol = 1)
        
        ## Perform the back propagation for pattern j
        
        gradient_weights <- back_pass(num_layers, weights, biases, x_pat, y_pat, trans_func, der_trans_func)[[1]]
        gradient_biases <- back_pass(num_layers, weights, biases, x_pat, y_pat, trans_func, der_trans_func)[[2]]
        
        for (k in 1:num_layers) {
            
            weights[[k]] <- weights[[k]] - ler_rate * gradient_weights[[k]]
            biases[[k]] <- biases[[k]] - ler_rate * gradient_biases[[k]]
            
        }
        
    }
    
    ## Record the error values for each training step
    
    if ((i * dim(x)[2]) %% 100 == 0) {
        
        err <- rms_error(D = t(y),
                         y = forward_pass(num_layers, weights, biases, x, trans_func)[[num_layers]])
        
        errors[length(errors) + 1] <- err
        ler_step[length(ler_step) + 1] <- i * dim(x)[2]
        input_vecs[[length(input_vecs) + 1]] <- x_pat
        expected_outputs[length(expected_outputs) + 1] <- y_pat
        actual_outputs[length(actual_outputs) + 1] <- forward_pass(num_layers, weights, biases, x, trans_func)[[num_layers]][dim(x)[2]]
        
    }
    
}

## Plot the RMSE over time

ggplot() +
    geom_line(aes(x = ler_step, y = errors)) +
    labs(x = "Learning Step", y = "RMS Error", title = "RMS Error per Learning Step")

## Plot the training desired vs. actual outputs

ggplot() +
    geom_line(aes(x = ler_step, y = expected_outputs - actual_outputs)) +
    labs(x = "Learning Step", y = "Difference, Desired vs. Actual", title = "Desired vs. Actual Output per Learning Step")













