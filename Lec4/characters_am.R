


## Read in the matrix

file <- "~/Documents/Rice_University/Spring_2018/NML502/Lec4/characters.ascii.txt"
data <- read.table(file)
data <- as.matrix(data)

## Load necessary packages

library(fields)

## Load in the err_corr function

source("~/Documents/Rice_University/Spring_2018/NML502/Lec4/err_corr.R")

## Load in a function to rotate the output

rotate <- function(x) t(apply(x, 2, rev))

## Construct the input/output matrix

data_matrix <- matrix(0, nrow = dim(data)[2], dim(data)[1])

for (i in 1:dim(data_matrix)[2]) {
    
    tmp_matrix <- matrix(data[i,], nrow = 7, ncol = 6, byrow = T)
    tmp_vec <- as.vector(tmp_matrix)
    data_matrix[,i] <- tmp_vec
    
}

## Set the parameters

mu <- 0.001
n <- 5000
tol <- 0.00000001

## Generate the memory matrix

err_corr(data_matrix, data_matrix, mu, n, tol)