## Load in necessary packages

library(ggplot2)

## Load in the necessary functions

source("~/Documents/Rice_University/Spring_2018/NML502/HW04_Part2/bp_learn.R")

## Define the back propogation recall function

bp_recall <- function() {
    
    ## Number of iterations
    
    num_iter <- 5000
    
    ## Learning rate for each layer
    
    ler_rate <- c(0.5, 0.000005)
    
    ## Batch size
    
    K <- 100
    
    ## Update the learning rate to match the batch size
    
    ler_rate <- ler_rate * (200 / K)
    
    ## Set the forget rate
    
    alpha <- c(0.04, 0.04)
    
    ## Define the hyperbolic tangent function
    
    trans_func <- function(x) {
        
        ((exp(1)^x) - (exp(1)^-x))/((exp(1)^x) + (exp(1)^-x))
        
    }
    
    ## Define the derivative of the hyperbolic tangent function
    
    der_trans_func <- function(x) {
        
        (1 - (trans_func(x)^2))
        
    }
    
    ## Define the function to learn
    
    f <- function(x) {
        
        1/x
        
    }
    
    ## Import the training data
    
    data_size <- 200
    x <- runif(n = 200, min = 0.1, max = 1)
    y <- f(x)/10
    
    ## Import the testing data
    
    x_test <- seq(from = 0.109, t = 1, by = 0.009)
    y_test <- f(x_test)/10
    
    ## Set up the network architecture
    
    num_outputs <- c(1, 10, 1)
    num_layers <- 2
    
    ## Train the network
    
    train_results <- bp_learn(num_iter, ler_rate, K, alpha, trans_func, der_trans_func, num_outputs, num_layers, x, y, 0.00000001, x_test, y_test)
    
    # Return the final network components
    
    weights <- train_results[[1]]
    biases <- train_results[[2]]
    ler_step <- train_results[[3]]
    errors_train <- train_results[[4]]
    errors_test <- train_results[[5]]
    output_diffs_train <- train_results[[6]]
    output_diffs_test <- train_results[[7]]
    y_output <- train_results[[8]]
    
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
    
}











