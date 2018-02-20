## Load in necessary packages

library(ggplot2)

## Load in the necessary functions

source("~/Documents/Rice_University/Spring_2018/NML502/HW04_Part2/bp_learn_iris.R")
# source("~/Documents/Rice_University/Spring_2018/NML502/HW04_Part2/network_test.R")

## Define the back propogation recall function

bp_recall_iris <- function() {
    
    ## Number of iterations
    
    num_iter <- 1000
    
    ## Learning rate for each layer
    
    ler_rate <- c(0.05, 0.05)
    
    ## Batch size
    
    K <- 10
    
    ## Update the learning rate to match the batch size
    
    ler_rate <- ler_rate
    
    ## Set the forget rate
    
    alpha <- c(0.4, 0.4)
    
    ## Define the hyperbolic tangent function
    
    trans_func <- function(x) {
        
        ((exp(1)^x) - (exp(1)^-x))/((exp(1)^x) + (exp(1)^-x))
        
    }
    
    ## Define the derivative of the hyperbolic tangent function
    
    der_trans_func <- function(x) {
        
        (1 - (trans_func(x)^2))
        
    }
    
    ## Load in the iris training data
    
    train_data <- read.table("~/Documents/Rice_University/Spring_2018/NML502/HW04_Part2/iris-train.txt", skip = 8)
    train_attr <- train_data[c(TRUE, FALSE), ]
    train_cats <- train_data[c(FALSE, TRUE), ][ ,2:4]
    
    train_data_final <- cbind(train_attr, train_cats)
    
    train_x <- transpose(train_data_final[, 1:4])
    train_y <- transpose(train_data_final[, 5:7])
    
    x <- matrix(as.numeric(unlist(train_x)), nrow = nrow(train_x))
    y <- matrix(as.numeric(unlist(train_y)), nrow = nrow(train_y))
    
    
    ## Set up the network architecture
    
    num_outputs <- c(4, 3, 3)
    num_layers <- 2
    
    ## Train the network
    
    train_results <- bp_learn_iris(num_iter, ler_rate, K, alpha, trans_func, der_trans_func, num_outputs, num_layers, x, y, 0.03)
    
    weights <- train_results[[1]]
    biases <- train_results[[2]]
    errors <- train_results[[3]]
    ler_step <- train_results[[4]]
    
    ## Test the data
    
    # test_results <<- network_test(weights, biases, num_layers, f, trans_func)
    
    # x_test <- test_results[[1]]
    # y_test_output <- test_results[[2]]
    # test_errors <- test_results[[3]]
    
    ## Plot the RMSE over time
    
    ggplot() +
        geom_line(aes(x = ler_step, y = errors), color = "blue") +
        labs(x = "Learning Step", y = "RMS Error", title = "RMS Error per Learning Step", subtitle = "Blue - Training, Red - Testing")
    
}











