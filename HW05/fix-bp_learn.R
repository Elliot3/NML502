
bp_learn <- function(num_iter, ler_rate, K, alpha, trans_func, der_trans_func, num_outputs, num_layers, x, y, tol, x_test, y_test) {
    
    ## Set the sixe of the inputs
    
    data_size <- length(x)
    
    ## Container for the errors and iteration number
    
    errors_train <- numeric()
    errors_test <- numeric()
    ler_step <- numeric()
    output_diffs_train <- numeric()
    output_diffs_test <- numeric()
    
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
        
        inds <- sample(1:length(x))
        
        ## Perform the permutation
        
        xi <- x[inds]
        yi <- y[inds]
        
        ## Create the gradient containers
        
        gradient_biases_sum <- list()
        gradient_weights_sum <- list()
        
        for (j in 1:K) {
            
            x_pat <<- as.matrix(xi[j])
            y_pat <<- as.matrix(yi[j])
            
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
            
            
            
            y_output <- forward_pass(num_layers, weights, biases, matrix(x, nrow = 1), trans_func)[[num_layers]]
            y_output_test <- forward_pass(num_layers, weights, biases, matrix(x_test, nrow = 1), trans_func)[[num_layers]]
                  
            
            
            #if (errors_train[iter] < tol) {
                
            #    ## Return the necesssary network information
                
            #    return(list(weights, biases, ler_step, errors_train, errors_test, output_diffs_train, output_diffs_test, y_output, y_output_test))
                
            #}
            
        }
        
    }
    
    ## Return the necesssary network information
    
    return(list(weights, biases, ler_step, errors_train, errors_test, output_diffs_train, output_diffs_test, y_output,y_output_test))
    
}