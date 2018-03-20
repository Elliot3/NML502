## Read in the necessary functions

source("~/Documents/Rice_University/Spring_2018/NML502/HW07/build_SOM.R")
source("~/Documents/Rice_University/Spring_2018/NML502/HW07/learn_map.R")

## Set the SOM lattice dimensions

matrix_dim <- 10

## Set the initial radius of influence on neighbors

radius <- matrix_dim / 2

## Set the number of iterations

num_iter <- 50000

## Set the initial learning rate

ler_rate <- 0.01

## Load in the input data

x_1 <- runif(n = 4000)
x_2 <- runif(n = 4000)

X <- rbind(x_1, x_2)
rownames(X) <- NULL

## Get the size of a typical input

input_size <- length(X[, 1])

## Build the SOM matrix

SOM_lattice <- build_SOM(input_size, matrix_dim)

## Perform the learning process

learn_results <- learn_map(SOM_lattice, X, num_iter, radius, ler_rate)



##### TESTING



## Plot the results

test_1 <- as.vector(learn_results[[1]])
test_2 <- as.vector(learn_results[[2]])

test_3 <- cbind(test_1, test_2)
rownames(test_3) <- NULL

plot(test_3)


