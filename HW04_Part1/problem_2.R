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
num_outputs <- c(1, 10, 1)

weights <- list()
biases <- list()

for (i in 1:num_layers) {
    
    weights[[i]] <- matrix(runif(num_outputs[i] * num_outputs[i + 1]), nrow = num_outputs[i + 1], ncol = num_outputs[i])
    biases[[i]] <- matrix(runif(num_outputs[i] * num_outputs[i + 1]), nrow = num_outputs[i + 1], ncol = 1)
    
}

## Import the training data

x <- runif(n = 200, min = 0.1, max = 1)
y <- 1/x
bias <- 1

## Initialize the training parameters

n <- 1000
ler_rate <- 0.001

## Set the epoch size

K <- 200

## Create the gradient containers

gradient_biases_sum <- list()
gradient_weights_sum <- list()

## Learn the weights via batch learning

for (i in 1:n) {
    
    for (j in 1:K) {
        
        x_pat <- as.matrix(x[j])
        y_pat <- as.matrix(y[j])
        
        if (j == 1) {
            
            back_pass(num_layers, weights, biases, x_pat, y_pat, trans_func, der_trans_func)
            
            gradient_biases_sum <- gradient_biases
            gradient_weights_sum <- gradient_weights
            
        } else {
            
            back_pass(num_layers, weights, biases, x_pat, y_pat, trans_func, der_trans_func)
            
        }
        
        for (k in 1:num_layers) {
            
            gradient_biases_sum[[k]] <- gradient_biases_sum[[k]] + gradient_biases[[k]]
            gradient_weights_sum[[k]] <- gradient_weights_sum[[k]] + gradient_weights[[k]]
            
        }
        
    }
    
    for (l in 1:num_layers) {
        
        weights[[l]] <- weights[[l]] - ler_rate * gradient_weights_sum[[l]]
        biases[[l]] <- biases[[l]] - ler_rate * gradient_biases_sum[[l]]
        
    }
    
    ## Record the error values for each training step
    
    if (TRUE == TRUE) {
        
        err <- rms_error(D = t(y),
                         y = forward_pass(num_layers, weights, biases, matrix(x, nrow = 1), trans_func)[[num_layers]])
        
        errors[length(errors) + 1] <- err
        ler_step[length(ler_step) + 1] <- i
        # input_vecs[[length(input_vecs) + 1]] <- x_pat
        # expected_outputs[length(expected_outputs) + 1] <- y_pat
        # actual_outputs[length(actual_outputs) + 1] <- forward_pass(num_layers, weights, biases, matrix(x, nrow = 1), trans_func)[[num_layers]]
        
    }
    
}

## Plot the RMSE over time

# ggplot() +
#     geom_line(aes(x = ler_step, y = errors)) +
#     labs(x = "Learning Step", y = "RMS Error", title = "RMS Error per Learning Step")

## Plot the training desired vs. actual outputs

# ggplot() +
#     geom_line(aes(x = ler_step, y = expected_outputs - actual_outputs)) +
#     labs(x = "Learning Step", y = "Difference, Desired vs. Actual", title = "Desired vs. Actual Output per Learning Step")













