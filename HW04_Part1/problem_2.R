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

errors_train <- numeric()
errors_test <- numeric()
ler_step <- numeric()
input_vecs <- list()
output_diffs_train <- numeric()
output_diffs_test <- numeric()



## Initialize the network

num_layers <- 2
num_outputs <- c(1, 10, 1)

weights <- list()
biases <- list()

for (i in 1:num_layers) {
    
    weights[[i]] <- matrix(runif(num_outputs[i] * num_outputs[i + 1],
                                 min = -0.1, max = 0.1), nrow = num_outputs[i + 1], ncol = num_outputs[i])
    biases[[i]] <- matrix(runif(num_outputs[i] * num_outputs[i + 1],
                                min = -0.1, max = 0.1), nrow = num_outputs[i + 1], ncol = 1)
    
}

## Import the training data

x <- runif(n = 200, min = 0.1, max = 1)
y <- (1/x)/10
bias <- 1

## Import the testing data

x_test <- seq(from = 0.109, t = 1, by = 0.009)
y_test <- (1/x_test)/10

## Initialize the training parameters

n <- 20000
ler_rate <- 0.01

## Set the epoch size

K <- 20

## Learn the weights via batch learning

for (i in 1:n) {
    
    ## Randomly permute the indices
    
    inds <- sample(1:length(x))
    
    ## Perform the permutation
    
    x <- x[inds]
    y <- y[inds]
    
    ## Create the gradient containers
    
    gradient_biases_sum <- list()
    gradient_weights_sum <- list()
    
    for (j in 1:K) {
        
        x_pat <<- as.matrix(x[j])
        y_pat <<- as.matrix(y[j])
        
        if (j == 1) {
            
            gradient_weights_sum <- back_pass(num_layers, weights, biases, x_pat, y_pat, trans_func, der_trans_func)[[1]]
            gradient_biases_sum <- back_pass(num_layers, weights, biases, x_pat, y_pat, trans_func, der_trans_func)[[2]]
            
        } else {
            
            gradient_weights <- back_pass(num_layers, weights, biases, x_pat, y_pat, trans_func, der_trans_func)[[1]]
            gradient_biases <- back_pass(num_layers, weights, biases, x_pat, y_pat, trans_func, der_trans_func)[[2]]
            
            for (k in 1:num_layers) {
                
                gradient_biases_sum[[k]] <- gradient_biases_sum[[k]] + gradient_biases[[k]]
                gradient_weights_sum[[k]] <- gradient_weights_sum[[k]] + gradient_weights[[k]]
                
            }
            
        }
        
    }
    
    for (l in 1:num_layers) {
        
        weights[[l]] <- weights[[l]] - ler_rate * gradient_weights_sum[[l]]
        biases[[l]] <- biases[[l]] - ler_rate * gradient_biases_sum[[l]]
        
    }
    
    ## Record the error values for each training step
    
    if (i %% 100 == 0) {
        
        if (exists("iter")) {
            iter <- iter + 1
        } else {
            iter <- 1
        }
        
        ler_step[length(ler_step) + 1] <- i
        
        
        
        err_train <- rms_error_batch(D = t(y),
                                     y = forward_pass(num_layers, weights, biases, matrix(x, nrow = 1), trans_func)[[num_layers]])
        errors_train[length(errors_train) + 1] <- err_train
        
        err_test <- rms_error_batch(D = t(y_test),
                                    y = forward_pass(num_layers, weights, biases, matrix(x_test, nrow = 1), trans_func)[[num_layers]])
        errors_test[length(errors_test) + 1] <- err_test
        
        
        
        y_expected_train <- y_pat
        y_actual_train <- forward_pass(num_layers, weights, biases, x_pat, trans_func)[[num_layers]]
        
        rand_ind <- sample(x = 1:length(x_test), size = 1)
        
        y_expected_test <- y_test[rand_ind]
        y_actual_test <- forward_pass(num_layers, weights, biases, matrix(x_test[rand_ind], nrow = 1), trans_func)[[num_layers]]
        
        output_diffs_train[length(output_diffs_train) + 1] <- y_expected_train - y_actual_train
        output_diffs_test[length(output_diffs_test) + 1] <- y_expected_test - y_actual_test
        
    }
    
}

y_output <- forward_pass(num_layers, weights, biases, matrix(x, nrow = 1), trans_func)[[num_layers]]

## Plot the RMSE over time

ggplot() +
    geom_line(aes(x = ler_step, y = errors_test), color = "red") +
    geom_line(aes(x = ler_step, y = errors_train), color = "blue") +
    labs(x = "Learning Step", y = "RMS Error", title = "Learning History", subtitle = "Blue - Training, Red - Testing")

## Plot the desired vs. actual outputs for test and training

ggplot() +
    geom_line(aes(x = ler_step, y = output_diffs_train), color = "blue") +
    geom_line(aes(x = ler_step, y = output_diffs_test), color = "red") +
    labs(x = "Learning Step", y = "Difference, Desired vs. Actual", title = "Desired vs. Actual Output per Learning Step", subtitle = "Blue - Training, Red - Testing")

## Plot the actual function vs. the learned function

ggplot() +
    geom_line(aes(x = x_test, y = y_test), color = "blue") +
    geom_line(aes(x = x, y = y_output), color = "red") +
    labs(x = "X Value", y = "Scaled Y Value", title = "Desired vs. Actual Output", subtitle = "Blue - True Function, Red - Learned Function")

