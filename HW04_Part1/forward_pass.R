## Create a container for the activations

activations <- list()

## Define the forward_pass function

forward_pass <- function(num_layers, weights, biases, x_pat, trans_func) {
    
    for (i in 1:num_layers) {
        
        if (i == 1) {
            
            activations[[i]] <- trans_func(weights[[i]] %*% x_pat + biases[[i]])
            
        } else {
            
            activations[[i]] <- trans_func(weights[[i]] %*% activations[[i - 1]] + biases[[i]])
            
        }
        
    }
    
    return(activations)
    
}