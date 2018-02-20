## Define the network test function

network_test <- function(weights, biases, num_layers, f , trans_func) {
    
    ## Import the test input data
    
    x <- seq(from = 0.15, to = 1, by = 0.00855)
    
    ## Get the desired values
    
    D <- f(x)
    
    ## Do forward pass for the actual values
    
    y <- forward_pass(num_layers, weights, biases, matrix(x, nrow = 1), trans_func)[[num_layers]] * 10
    
    ## Calculate the error
    
    error <- D - y
    
    ## Return the necessary results
    
    return(list(x, y, error))
    
}