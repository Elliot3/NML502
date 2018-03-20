## Read in the necessary functions

source("~/Documents/Rice_University/Spring_2018/NML502/HW07/build_SOM.R")
source("~/Documents/Rice_University/Spring_2018/NML502/HW07/learn_map.R")

## Set the SOM lattice dimensions

matrix_dim <- 8

## Set the initial radius of influence on neighbors

radius <- matrix_dim / 2

## Set the number of iterations

num_iter <- 150000

## Set the initial learning rate

ler_rate <- 0.01

## Load in the input data

x_1 <- rnorm(n = 1000, mean = 7, sd = sqrt(0.1))
x_2 <- rnorm(n = 1000, mean = 7, sd = sqrt(0.1))

x_3 <- rnorm(n = 1000, mean = 0, sd = sqrt(0.1))
x_4 <- rnorm(n = 1000, mean = 7, sd = sqrt(0.1))

x_5 <- rnorm(n = 1000, mean = 7, sd = sqrt(0.1))
x_6 <- rnorm(n = 1000, mean = 0, sd = sqrt(0.1))

x_7 <- rnorm(n = 1000, mean = 0, sd = sqrt(0.1))
x_8 <- rnorm(n = 1000, mean = 0, sd = sqrt(0.1))

X_1 <- rbind(x_1, x_2)
X_2 <- rbind(x_3, x_4)
X_3 <- rbind(x_5, x_6)
X_4 <- rbind(x_7, x_8)

X <- cbind(X_1, X_2, X_3, X_4)

rownames(X) <- NULL

## Get the size of a typical input

input_size <- length(X[, 1])

## Build the SOM matrix

SOM_lattice <- build_SOM(input_size, matrix_dim)

## Perform the learning process

learn_results <- learn_map(SOM_lattice, X, num_iter, radius, ler_rate)

## Plot the results

plot_SOM(learn_results)
