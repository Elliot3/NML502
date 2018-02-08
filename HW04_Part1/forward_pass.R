## Create a container for the activations

activations <- list()

## Define the forward_pass function

forward_pass <- function(num_layers, weights, biases, x, act_func) {
    
    for (i in 1:num_layers) {
        
        if (i == 1) {
            
            activations[[i]] <- act_func(weights[[i]] %*% x + biases[[i]])
            
        } else {
            
            activations[[i]] <- act_func(weights[[i]] %*% activations[[i - 1]] + biases[[i]])
            
        }
        
    }
    
    return(activations)
    
}