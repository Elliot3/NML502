
    
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
# nonlinear com channel
x_train_mod <- ncc(x_train)/3


test_func_one <- function(x) {
    
    0.8*sin((2*pi*x)/10) + 0.25*cos((2*pi*x)/25)
    
}
x_test <- test_func_one(time)

x_test_mod <- ncc(x_test)/3




# xx <- ncc(dd)
# ## Scale the test data down
# xx <- xx / 3

# Forward pass





# Array for number of outputs
num_outputs <- c(1, 200, 1)
num_layers <- 2

# Train the network
train_results <- bp_learn(num_iter, ler_rate, K, alpha, trans_func, der_trans_func, num_outputs, num_layers, x_train, x_train_mod, 0.02, x_test, x_test_mod)

# Return the final network components

weights <- train_results[[1]]
biases <- train_results[[2]]
ler_step <- train_results[[3]]
errors_train <- train_results[[4]]
errors_test <- train_results[[5]]
output_diffs_train <- train_results[[6]]
output_diffs_test <- train_results[[7]]
y_output <- train_results[[8]]
y_output_test <- train_results[[9]]
## Plot the RMSE over time

ggplot() +
    geom_line(aes(x = ler_step, y = errors_test), color = "red") +
    geom_line(aes(x = ler_step, y = errors_train), color = "blue") +
    labs(x = "Learning Step", y = "RMS Error", title = "Learning History", subtitle = "Blue - Training, Red - Testing")

## Plot the desired vs. actual outputs for test and training

ggplot() +
    geom_line(aes(x = ler_step, y = output_diffs_train), color = "blue") +
    geom_line(aes(x = ler_step, y = output_diffs_test), color = "red") +
    labs(x = "Learning Step", y = "Difference, Desired vs. Actual", title = "Desired vs. Actual Output per Learning Step", subtitle = "Blue - Training, Red - Testing")

## Plot the actual function vs. the learned function

ggplot() +
    geom_line(aes(x = time, y = x_train_mod), color = "blue") +
    geom_line(aes(x = time, y = y_output), color = "red") +
    labs(x = "X Value", y = "Scaled Y Value", title = "Desired vs. Actual Output", subtitle = "Blue - True Function, Red - Learned Function")





######input

ggplot() +
    geom_line(aes(x = time, y = x_test), color = "blue") +
    geom_line(aes(x = time, y = x_test_mod), color = "red") +
    labs(x = "X Value", y = "Scaled Y Value", title = "Output", subtitle = "Blue - True Function, Red - NCC channel")



ggplot() +
    geom_line(aes(x = time, y = x_train), color = "blue") +
    geom_line(aes(x = time, y = x_train_mod), color = "red") +
    labs(x = "X Value", y = "Scaled Y Value", title = "Output", subtitle = "Blue - True Function, Red - NCC channel")
