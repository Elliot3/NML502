#main commands in R, first define input, output, then call bp_learn, then plot,  at the bottom at the functions bp_learn and forward and backward functions


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
    
    2 * sin((2*pi*x)/20)
    
}

# Nonlinear comm channel function
ncc <- function(s) {
    
    temp <- s + 0.2*(s^2)
    return(temp)
    
}

# Traing data
data_size <- 1001

time <- seq(from = -50, to = 50, by = 0.1)

x_train <- f(time)
x_train_sca <- x_train/3
# nonlinear com channel
x_train_mod <- ncc(x_train)/3


test_func_one <- function(x) {
    
    0.8*sin((2*pi*x)/10) + 0.25*cos((2*pi*x)/25)
    
}

x_test <- test_func_one(time)
x_test_sca <- x_test/3

x_test2 <- rnorm(1001,0,1)
x_test2_sca <- x_test2/3

#scale
x_test_mod <- ncc(x_test)/3
x_test2_mod <- ncc(x_test2)/3



# xx <- ncc(dd)
# ## Scale the test data down
# xx <- xx / 3

# Forward pass



   
# Number of iterations
num_iter <- 10000

# Learning rate by layer
ler_rate <- c(0.018, 0.003)

# Batch size
K <- 20

# Update learning rate to batch size
ler_rate <- ler_rate * (200 / K)

# Forget rate
alpha <- c(0.9, 0.9)

# Array for number of outputs
num_outputs <- c(1, 100, 1)
num_layers <- 2

# Train the network
train_results <- bp_learn(num_iter, ler_rate, K, alpha, trans_func, der_trans_func, num_outputs, num_layers, x_train_sca, x_train_mod, 0.02, x_test_sca, x_test_mod)

weights <- train_results[[1]]
biases <- train_results[[2]]
ler_step <- train_results[[3]]
errors_train <- train_results[[4]]
errors_test <- train_results[[5]]
output_diffs_train <- train_results[[6]]
output_diffs_test <- train_results[[7]]
y_output <- train_results[[8]]
y_output_test <- train_results[[9]]

train_results <- bp_learn(num_iter, ler_rate, K, alpha, trans_func, der_trans_func, num_outputs, num_layers, x_train_sca, x_train_mod, 0.02, x_test2_sca, x_test2_mod)

y_output2 <- train_results[[8]]
y_output2_test <- train_results[[9]]
# Return the final network components


## Plot the RMSE over time

ggplot() +
    geom_line(aes(x = ler_step, y = errors_test^2), color = "red",size = 1) +
    geom_line(aes(x = ler_step, y = errors_train^2), color = "blue",size = 1) +
    theme_bw() +
    labs(x = "Learning Step", y = "MSE Error", title = "Learning History", subtitle = "Blue - Training, Red - Testing")

## Plot the desired vs. actual outputs for test and training

ggplot() +
    geom_line(aes(x = ler_step, y = output_diffs_train), color = "blue") +
    geom_line(aes(x = ler_step, y = output_diffs_test), color = "red") +
    labs(x = "Learning Step", y = "Difference, Desired vs. Actual", title = "Desired vs. Actual Output per Learning Step", subtitle = "Blue - Training, Red - Testing")

## Plot the actual function vs. the learned function

ggplot() +
    geom_line(aes(x = time, y = x_train_sca), color = "green",size = 1) +
    geom_line(aes(x = time, y = x_train_mod), color = "blue",size = 1) +
    geom_line(aes(x = time, y = y_output), color = "red",size = 1) +
    theme_bw() +
    labs(x = "n", y = "output", title = "Desired vs. Actual Output", subtitle = "Green -original function, Blue - True Function, Red - Learned Function")

ggplot() +
    geom_line(aes(x = time, y = x_test_sca), color = "green",size = 1) +
    geom_line(aes(x = time, y = x_test_mod), color = "blue",size = 1) +
    geom_line(aes(x = time, y = y_output_test), color = "red",size = 1) +
    theme_bw() +
    labs(x = "n", y = "output", title = "Desired vs. Actual Output", subtitle = "Green -original function, Blue - True Function, Red - Learned Function")

ggplot() +
    geom_line(aes(x = time, y = x_test2_sca), color = "green",size = 1) +
    geom_line(aes(x = time, y = x_test2_mod), color = "blue",size = 1) +
    geom_line(aes(x = time, y = y_output2_test), color = "red",size = 1) +
    theme_bw() +
    labs(x = "n", y = "output", title = "Desired vs. Actual Output", subtitle = "Green -original function, Blue - True Function, Red - Learned Function")


##############################################




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



###########################################
## Create a container for the activations

activations <- list()

## Define the forward_pass function

forward_pass <- function(num_layers, weights, biases, x_pat, trans_func) {
    
    for (i in 1:num_layers) {
        
        if (i == 1) {
            
            activations[[i]] <- apply(x_pat, 2, function(z) {
                
                trans_func(weights[[i]] %*% z + biases[[i]])
                
            })
            
            # activations[[i]] <- trans_func(weights[[i]] %*% x_pat + biases[[i]])
            
        } else {
            
            activations[[i]] <- apply(activations[[i - 1]], 2, function(z) {
                
                trans_func(weights[[i]] %*% z + biases[[i]])
                
            })
            
            # activations[[i]] <- trans_func(weights[[i]] %*% activations[[i - 1]] + biases[[i]])
            
        }
        
    }
    
    return(activations)
    
}

## Define the back_pass function

back_pass <- function(num_layers, weights, biases, x_pat, y_pat, trans_func, der_trans_func) {
    
    # Create containers for the gradients
    
    gradient_biases <- list()
    gradient_weights <- list()
    
    ## Get the activations from a forward pass
    
    activations <- forward_pass(num_layers, weights, biases, x_pat, trans_func)
    
    ## Get the error of the last layer
    
    delta <- (activations[[num_layers]] - y_pat) * der_trans_func(activations[[num_layers]])
    
    ## Set the gradients of the last layer
    
    gradient_biases[[num_layers]] <- delta
    gradient_weights[[num_layers]] <- delta %*% t(activations[[num_layers - 1]])
    
    ## Calculate the gradients of all other layers
    
    for (i in seq(from = num_layers - 1, to = 1, by = -1)) {
        
        delta <- (t(weights[[i + 1]]) %*% delta) * der_trans_func(activations[[i]])
        
        gradient_biases[[i]] <- delta
        
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

## Define the root mean square error function

rms_error <- function(D, y) {
    
    rms <- sqrt(sum((D - y)^2))
    return(rms)
    
}

rms_error_batch <- function(D, y) {
    
    rms <- sqrt(sum((D - y)^2)/length(y))
    return(rms)
    
}