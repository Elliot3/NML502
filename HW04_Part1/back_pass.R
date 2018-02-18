## Define the back_pass function

back_pass <- function(num_layers, weights, biases, x_pat, y_pat, trans_func, der_trans_func) {
    
    # Create containers for the gradients
    
    gradient_biases <- list()
    gradient_weights <- list()
    
    ## Get the activations from a forward pass
    
    activations <- forward_pass(num_layers, weights, biases, x_pat, trans_func)
    
    ## Get the error of the last layer
    
    delta <- (activations[[num_layers]] - y_pat) %*% der_trans_func(activations[[num_layers]])
    
    ## Set the gradients of the last layer
    
    gradient_biases[[num_layers]] <- delta
    gradient_weights[[num_layers]] <- delta %*% t(activations[[num_layers - 1]])
    
    ## Calculate the gradients of all other layers
    
    for (i in seq(from = num_layers - 1, to = 1, by = -1)) {
        
        delta <- (t(weights[[i + 1]]) %*% delta) * der_trans_func(activations[[i]])
        
        gradient_biases <- delta
        
        if (i != 1) {
            
            gradient_weights[[i]] <- delta %*% t(activations[[i - 1]])
            
        } else {
            
            gradient_weights[[i]] <- delta %*% t(x_pat)
            
        }
        
    }
    
    ## Save the gradients to the global environment
    
    # gradient_weights <<- gradient_weights
    # gradient_biases <<- gradient_biases
    
    return(list(gradient_weights, gradient_biases))
    
}