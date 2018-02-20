## Load in the necessary functions

source("~/Documents/Rice_University/Spring_2018/NML502/HW04_Part1/forward_pass.R")
source("~/Documents/Rice_University/Spring_2018/NML502/HW04_Part1/back_pass.R")
source("~/Documents/Rice_University/Spring_2018/NML502/HW04_Part1/rms_error.R")

## Define the back propogation learning function

bp_learn_iris <- function(num_iter, ler_rate, K, alpha, trans_func, der_trans_func, num_outputs, num_layers, x, y, tol) {
    
    ## Set the sixe of the inputs
    
    data_size <- dim(x)[2]
    
    ## Container for the errors and iteration number
    
    errors <- numeric()
    ler_step <- numeric()
    
    ## Containers for the weights and biases
    
    weights <- list()
    biases <- list()
    
    ## Container for previous weight updates
    
    weights_prev_delta <- list()
    biases_prev_delta <- list()
    
    ## Create the weights and biases and init the previous deltas
    
    for (i in 1:num_layers) {
        
        a <- -1/10
        b <- 1/10
        
        ## Create the weights
        
        weights[[i]] <- (b - a) * matrix(runif(num_outputs[i] * num_outputs[i + 1], min = -0.1, max = 0.1), nrow = num_outputs[i + 1], ncol = num_outputs[i]) + a
        biases[[i]] <- (b -a) * matrix(runif(num_outputs[i] * num_outputs[i + 1], min = -0.1, max = 0.1), nrow = num_outputs[i + 1], ncol = 1) + a
        
        ## Initialize the previous deltas
        
        weights_prev_delta[[i]] <- matrix(0, nrow = num_outputs[i + 1], ncol = num_outputs[i])
        biases_prev_delta[[i]] <- matrix(0, nrow = num_outputs[i + 1], ncol = 1)
        
    }
    
    ## Train the network
    
    for (i in 1:num_iter) {
        
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
        
        ## Make the network updates
        
        for (l in 1:num_layers) {
            
            ## Update the weights
            
            weights[[l]] <- weights[[l]] - ler_rate[l] * gradient_weights_sum[[l]] + alpha[l] * weights_prev_delta[[l]]
            biases[[l]] <- biases[[l]] - ler_rate[l] * gradient_biases_sum[[l]] + alpha[l] * biases_prev_delta[[l]]
            
            ## Store the changes
            
            weights_prev_delta[[l]] <- -ler_rate[l] * gradient_weights_sum[[l]]
            biases_prev_delta[[l]] <- -ler_rate[l] * gradient_biases_sum[[l]]
            
        }
        
        ## Perform the forward pass
        
        y_train <- forward_pass(num_layers, weights, biases, matrix(x, nrow = 1), trans_func)[[num_layers]]
        
        y_max <- max(y_train)
        
        ## Calculate the error
        
        errors[i] <- sum(sum(abs(y_max - y)))
        
        ## Store the iteration number
        
        ler_step[i] <- i
        
        ## Stop the training if tolerance is met
        
        if (errors[i] < 6) {
            
            break
            
        }
        
    }
    
    ## Return the necesssary network information
    
    return(list(weights, biases, errors, ler_step))
    
}