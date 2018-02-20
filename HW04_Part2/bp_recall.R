## Load in the necessary functions

source("~/Documents/Rice_University/Spring_2018/NML502/HW04_Part2/bp_learn.R")

## Define the back propogation recall function

bp_recall <- function() {
    
    ## Number of iterations
    
    num_iter <- 500
    
    ## Learning rate for each layer
    
    ler_rate <- c(0.018, 0.003)
    
    ## Batch size
    
    K <- 10
    
    ## Update the learning rate to match the batch size
    
    ler_rate <- ler_rate * (200 / K)
    
    ## Set the forget rate
    
    alpha <- c(0.004, 0.004)
    
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
    
    ## Set up the network architecture
    
    num_outputs <- c(1, 10, 1)
    num_layers <- 2
    
    ## Train the network
    
    bp_learn(num_iter, ler_rate, K, alpha, trans_func, der_trans_func, num_outputs, num_layers, x, y, 0.1)
    
}











