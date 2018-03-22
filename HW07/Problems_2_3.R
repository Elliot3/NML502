## No scientific notation

options(scipen = 999)

## Load in the necessary libraries

library(plotly)

## Read in the necessary functions

source("~/Documents/Rice_University/Spring_2018/NML502/HW07/build_SOM.R")
source("~/Documents/Rice_University/Spring_2018/NML502/HW07/learn_map.R")
source("~/Documents/Rice_University/Spring_2018/NML502/HW07/plot_SOM.R")
source("~/Documents/Rice_University/Spring_2018/NML502/HW07/recall_PE.R")



##### Problem 2 #####



## Set the SOM lattice dimensions

matrix_dim <- 8

## Set the initial radius of influence on neighbors

radius <- matrix_dim / 2

## Set the number of iterations

num_iter <- 500000

## Set the initial learning rate

ler_rate <- 0.001

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

par(mfrow = c(3, 2))
par(mar = c(3, 3, 3, 3))
plot_SOM(learn_results)

## Plot the decay of the radius and learning rate

plot(x = param_container[[1]], y = param_container[[2]], type = "l",
     xlab = "Learning Step",
     ylab = "Neighborhood Radius",
     main = "Neighborhood Radius Decay")

plot(x = param_container[[1]], y = param_container[[3]], type = "l",
     xlab = "Learning Step",
     ylab = "Learning Rate",
     main = "Learning Rate Decay")

## Perform the recall

recall_PE(learn_results, X)



##### Problem 3 #####



## Unwrap the final PE prototypes

x_lattice <- learn_results[[length(learn_results)]][[2]][[1]]
y_lattice <- learn_results[[length(learn_results)]][[2]][[2]]

## Scale the PEs down and vectorize for plotting purposes

scale_x <- scale(c(t(x_lattice)))
scale_y <- scale(c(t(y_lattice)))

## Generate the frame and add the plots

par(mfrow = c(matrix_dim, matrix_dim))
par(mar = c(0, 0, 0, 0))

for (i in 1:length(x_lattice)) {
    
    plot(x = 1:input_size,
         y = c(scale_x[i], scale_y[i]),
         type = "l",
         xaxt = "n",
         yaxt = "n",
         ann = FALSE,
         xlim = c(-3, 3),
         ylim = c(-3, 3))
    
}



