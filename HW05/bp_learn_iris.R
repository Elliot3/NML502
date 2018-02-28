## Load in the necessary functions

source("~/Documents/Rice_University/Spring_2018/NML502/HW04_Part1/forward_pass.R")
source("~/Documents/Rice_University/Spring_2018/NML502/HW04_Part1/back_pass.R")
source("~/Documents/Rice_University/Spring_2018/NML502/HW04_Part1/rms_error.R")

## Define the back propogation learning function

bp_learn_iris <- function(num_iter, ler_rate, K, alpha, trans_func, der_trans_func, num_outputs, num_layers, x, y, tol, x_test, y_test) {
    
    ## Set the sixe of the inputs
    if (exists("iter")) {
    rm(iter)}
    data_size <- dim(x)[2]
    
    ## Container for the errors and iteration number
    
    errors_train <- numeric()
    errors_test <- numeric()
    confusion_matrix_train <- matrix(nrow=3, ncol=3)
    confusion_matrix_test <- matrix(nrow=3, ncol=3)
    #y_train_out_arr <- numeric()
    #y_test_out_arr <- numeric()
    ler_step <- numeric()
    
    ## Containers for the weights and biases
    
    weights <- list()
    biases <- list()
    
    ## Container for previous weight updates
    
    weights_prev_delta <- list()
    biases_prev_delta <- list()
    
    ## Create the weights and biases and init the previous deltas
    
    for (i in 1:num_layers) {
        
        ## Create the weights
        
        weights[[i]] <- matrix(runif(num_outputs[i] * num_outputs[i + 1],
                                     min = -0.1, max = 0.1), nrow = num_outputs[i + 1], ncol = num_outputs[i])
        biases[[i]] <- matrix(runif(num_outputs[i] * num_outputs[i + 1],
                                    min = -0.1, max = 0.1), nrow = num_outputs[i + 1], ncol = 1)
        
        ## Initialize the previous deltas
        
        weights_prev_delta[[i]] <- matrix(0, nrow = num_outputs[i + 1], ncol = num_outputs[i])
        biases_prev_delta[[i]] <- matrix(0, nrow = num_outputs[i + 1], ncol = 1)
        
    }
    
    ## Train the network
    
    for (i in 1:num_iter) {
        
        ## Randomly permute the indices
        
        inds <- sample(1:dim(x)[2])
        
        ## Perform the permutation
        
        x <- matrix(x[, inds], nrow = 4)
        y <- matrix(y[, inds], nrow = 3)
        
        ## Create the gradient containers
        
        gradient_biases_sum <- list()
        gradient_weights_sum <- list()
        
        for (j in 1:K) {
            
            x_pat <<- as.matrix(x[, j], nrow = 4)
            y_pat <<- as.matrix(y[, j], nrow = 3)
            
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
        
        ## Record the error values for each training step
        
        if (i %% 100 == 0) {
            
            if (exists("iter")) {
                iter <- iter + 1
            } else {
                iter <- 1
            }
            
            ler_step[length(ler_step) + 1] <- i
            
            ## Perform the forward pass on the training data
            
            y_train_out <- forward_pass(num_layers, weights, biases, x, trans_func)[[num_layers]]
            
            y_max_train <- max(y_train_out)
            
            ## Perform the forward pass on the training data
            
            y_test_out <- forward_pass(num_layers, weights, biases, x_test, trans_func)[[num_layers]]
            
            y_max_test <- max(y_test_out)
            
            ## Calculate the errors
            #y_train_out_arr[iter] <- y_train_out
            #y_test_out_arr[iter] <- y_test_out
            errors_train[iter] <- sum(sum(abs(y - y_train_out)))
            errors_test[iter] <- sum(sum(abs(y_test - y_test_out)))
            #confusion_matrix_train
            #confusion_matrix_test
            print(iter)
            print(errors_test)
            
            #if (errors_train[iter] < 6) {
                
                ## Return the necesssary network information
                
            #return(list(weights, biases, ler_step, errors_train, errors_test))
                
            #}
            
        }
        
    }
    
    ## Return the necesssary network information
    
    return(list(weights, biases, ler_step, errors_train, errors_test, confusion_matrix_train, confusion_matrix_test))
    
}