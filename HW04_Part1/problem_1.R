## Load in necessary functions

source("~/Documents/Rice_University/Spring_2018/NML502/HW04_Part1/forward_pass.R")

## Define the hyperbolic tangent function

hyp_tan <- function(x) {
    
    ((exp(1)^x) - (exp(1)^-x))/((exp(1)^x) + (exp(1)^-x))
    
}

## Define the derivative of the hyperbolic tangent function

der_hyp_tan <- function(x) {
    
    (1 - (hyp_tan(x)^2))
    
}

## Initialize the network

num_layers <- 2
num_outputs <- c(2, 2, 1)

weights <- list()
biases <- list()

for (i in 1:num_layers) {
    
    weights[[i]] <- matrix(runif(num_outputs[i] * num_outputs[i + 1]), nrow = num_outputs[i + 1], ncol = num_outputs[i])
    biases[[i]] <- matrix(runif(num_outputs[i] * num_outputs[i + 1]), nrow = num_outputs[i + 1], ncol = 1)
    
}

## Initialize the training parameters

n <- 1000
ler_rate <- 0.001

## Import the training data

x <- matrix(c(1,1,0,0,1,0,1,0), nrow = 2, ncol = 4, byrow = T)
y <- matrix(c(0,1,1,0), nrow = 1, ncol = 4)
bias <- matrix(rep(1, 4), nrow = 1, ncol = 4)
