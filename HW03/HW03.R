


## Load packages

library(fields)

## Load in the data

E <- matrix(nrow = 12, ncol = 12)

E[1,] <- c(1,1,1,1,1,1,1,0,0,0,0,0)
E[2,] <- c(1,1,1,1,1,1,1,0,0,0,0,0)
E[3,] <- c(1,1,0,0,0,0,0,0,0,0,0,0)
E[4,] <- c(1,1,0,0,0,0,0,0,0,0,0,0)
E[5,] <- c(1,1,0,0,0,0,0,0,0,0,0,0)
E[6,] <- c(1,1,1,1,1,1,0,0,0,0,0,0)
E[7,] <- c(1,1,1,1,1,1,0,0,0,0,0,0)
E[8,] <- c(1,1,0,0,0,0,0,0,0,0,0,0)
E[9,] <- c(1,1,0,0,0,0,0,0,0,0,0,0)
E[10,] <- c(1,1,0,0,0,0,0,0,0,0,0,0)
E[11,] <- c(1,1,1,1,1,1,1,0,0,0,0,0)
E[12,] <- c(1,1,1,1,1,1,1,0,0,0,0,0)

H <- matrix(nrow = 12, ncol = 12)

H[1,] <- c(0,0,1,1,0,0,0,0,1,1,0,0)
H[2,] <- c(0,0,1,1,0,0,0,0,1,1,0,0)
H[3,] <- c(0,0,1,1,0,0,0,0,1,1,0,0)
H[4,] <- c(0,0,1,1,0,0,0,0,1,1,0,0)
H[5,] <- c(0,0,1,1,0,0,0,0,1,1,0,0)
H[6,] <- c(0,0,1,1,1,1,1,1,1,1,0,0)
H[7,] <- c(0,0,1,1,1,1,1,1,1,1,0,0)
H[8,] <- c(0,0,1,1,0,0,0,0,1,1,0,0)
H[9,] <- c(0,0,1,1,0,0,0,0,1,1,0,0)
H[10,] <- c(0,0,1,1,0,0,0,0,1,1,0,0)
H[11,] <- c(0,0,1,1,0,0,0,0,1,1,0,0)
H[12,] <- c(0,0,1,1,0,0,0,0,1,1,0,0)

T <- matrix(nrow = 12, ncol = 12)

T[1,] <- c(0,1,1,1,1,1,1,1,1,1,1,0)
T[2,] <- c(0,1,1,1,1,1,1,1,1,1,1,0)
T[3,] <- c(0,0,0,0,0,1,1,0,0,0,0,0)
T[4,] <- c(0,0,0,0,0,1,1,0,0,0,0,0)
T[5,] <- c(0,0,0,0,0,1,1,0,0,0,0,0)
T[6,] <- c(0,0,0,0,0,1,1,0,0,0,0,0)
T[7,] <- c(0,0,0,0,0,1,1,0,0,0,0,0)
T[8,] <- c(0,0,0,0,0,1,1,0,0,0,0,0)
T[9,] <- c(0,0,0,0,0,1,1,0,0,0,0,0)
T[10,] <- c(0,0,0,0,0,1,1,0,0,0,0,0)
T[11,] <- c(0,0,0,0,0,1,1,0,0,0,0,0)
T[12,] <- c(0,0,0,0,0,1,1,0,0,0,0,0)

O <- matrix(nrow = 12, ncol = 12)

O[1,] <- c(0,0,0,1,1,1,1,1,1,0,0,0)
O[2,] <- c(0,0,1,1,1,1,1,1,1,1,0,0)
O[3,] <- c(0,1,1,1,0,0,0,0,1,1,1,0)
O[4,] <- c(0,1,1,1,0,0,0,0,1,1,1,0)
O[5,] <- c(0,1,1,1,0,0,0,0,1,1,1,0)
O[6,] <- c(0,1,1,1,0,0,0,0,1,1,1,0)
O[7,] <- c(0,1,1,1,0,0,0,0,1,1,1,0)
O[8,] <- c(0,1,1,1,0,0,0,0,1,1,1,0)
O[9,] <- c(0,1,1,1,0,0,0,0,1,1,1,0)
O[10,] <- c(0,1,1,1,0,0,0,0,1,1,1,0)
O[11,] <- c(0,0,1,1,1,1,1,1,1,1,0,0)
O[12,] <- c(0,0,0,1,1,1,1,1,1,0,0,0)

M <- matrix(nrow = 12, ncol = 12)

M[1,] <- c(1,1,1,0,0,0,0,0,1,1,1,0)
M[2,] <- c(1,1,1,1,0,0,0,1,1,1,1,0)
M[3,] <- c(1,1,1,1,1,0,1,1,1,1,1,0)
M[4,] <- c(1,1,0,1,1,1,1,1,0,1,1,0)
M[5,] <- c(1,1,0,0,1,1,1,0,0,1,1,0)
M[6,] <- c(1,1,0,0,0,1,0,0,0,1,1,0)
M[7,] <- c(1,1,0,0,0,0,0,0,0,1,1,0)
M[8,] <- c(1,1,0,0,0,0,0,0,0,1,1,0)
M[9,] <- c(1,1,0,0,0,0,0,0,0,1,1,0)
M[10,] <- c(1,1,0,0,0,0,0,0,0,1,1,0)
M[11,] <- c(1,1,0,0,0,0,0,0,0,1,1,0)
M[12,] <- c(1,1,0,0,0,0,0,0,0,1,1,0)

E[E == 0] <- -1
H[H == 0] <- -1
T[T == 0] <- -1
O[O == 0] <- -1
M[M == 0] <- -1

## Load in the err_corr function

source("~/Documents/Rice_University/Spring_2018/NML502/Lec4/err_corr.R")

## Vectorize the matrices and prepare the input

E_vec <- as.vector(E)
H_vec <- as.vector(H)
T_vec <- as.vector(T)
O_vec <- as.vector(O)
M_vec <- as.vector(M)

input <- cbind(E_vec, H_vec, T_vec, O_vec, M_vec)

mu <- 0.001
n <- 1000
tol <- 0.0000001



########## Problem 3



##### Problem a

## Call the err_corr function

err_corr(input, input, mu, n, tol)

## Load in a function to rotate the output

rotate <- function(x) t(apply(x, 2, rev))

## Output the original image

image.plot(rbind(rotate(E), rotate(H), rotate(T), rotate(O), rotate(M)), col = heat.colors(12))

## Recall the images from memory

E_recall <- matrix(mem_matrix %*% E_vec, nrow = 12, ncol = 12)
H_recall <- matrix(mem_matrix %*% H_vec, nrow = 12, ncol = 12)
T_recall <- matrix(mem_matrix %*% T_vec, nrow = 12, ncol = 12)
O_recall <- matrix(mem_matrix %*% O_vec, nrow = 12, ncol = 12)
M_recall <- matrix(mem_matrix %*% M_vec, nrow = 12, ncol = 12)

## Rebuild the image

image.plot(rbind(rotate(E_recall), rotate(H_recall), rotate(T_recall), rotate(O_recall), rotate(M_recall)), col = heat.colors(12))



##### Problem b



## Corrupt 25% of from each input pattern

inds <- sort(sample(1:144, 36, replace = FALSE))

E_vec_corr_25 <- E_vec
E_vec_corr_25[inds] <- E_vec_corr_25[inds] * (-1)

inds <- sort(sample(1:144, 36, replace = FALSE))

H_vec_corr_25 <- H_vec
H_vec_corr_25[inds] <- H_vec_corr_25[inds] * (-1)

inds <- sort(sample(1:144, 36, replace = FALSE))

T_vec_corr_25 <- T_vec
T_vec_corr_25[inds] <- T_vec_corr_25[inds] * (-1)

inds <- sort(sample(1:144, 36, replace = FALSE))

O_vec_corr_25 <- O_vec
O_vec_corr_25[inds] <- O_vec_corr_25[inds] * (-1)

inds <- sort(sample(1:144, 36, replace = FALSE))

M_vec_corr_25 <- M_vec
M_vec_corr_25[inds] <- M_vec_corr_25[inds] * (-1)

## Output the original, corrupted image

image.plot(rbind(rotate(matrix(E_vec_corr_25, nrow = 12, ncol = 12)), 
                 rotate(matrix(H_vec_corr_25, nrow = 12, ncol = 12)),
                 rotate(matrix(T_vec_corr_25, nrow = 12, ncol = 12)),
                 rotate(matrix(O_vec_corr_25, nrow = 12, ncol = 12)),
                 rotate(matrix(M_vec_corr_25, nrow = 12, ncol = 12))),
           col = heat.colors(12))

## Prepare the 25% corrupted input

input_corr_25 <- cbind(E_vec_corr_25, H_vec_corr_25, T_vec_corr_25, O_vec_corr_25, M_vec_corr_25)

## Recall the 25% corrupted images from memory

E_25_recall <- matrix(mem_matrix %*% E_vec_corr_25, nrow = 12, ncol = 12)
H_25_recall <- matrix(mem_matrix %*% H_vec_corr_25, nrow = 12, ncol = 12)
T_25_recall <- matrix(mem_matrix %*% T_vec_corr_25, nrow = 12, ncol = 12)
O_25_recall <- matrix(mem_matrix %*% O_vec_corr_25, nrow = 12, ncol = 12)
M_25_recall <- matrix(mem_matrix %*% M_vec_corr_25, nrow = 12, ncol = 12)

## Rebuild the image

image.plot(rbind(rotate(E_25_recall), rotate(H_25_recall), rotate(T_25_recall), rotate(O_25_recall), rotate(M_25_recall)), col = heat.colors(12))



## Corrupt 50% of from each input pattern

inds <- sort(sample(1:144, 72, replace = FALSE))

E_vec_corr_50 <- E_vec
E_vec_corr_50[inds] <- E_vec_corr_50[inds] * (-1)

inds <- sort(sample(1:144, 72, replace = FALSE))

H_vec_corr_50 <- H_vec
H_vec_corr_50[inds] <- H_vec_corr_50[inds] * (-1)

inds <- sort(sample(1:144, 72, replace = FALSE))

T_vec_corr_50 <- T_vec
T_vec_corr_50[inds] <- T_vec_corr_50[inds] * (-1)

inds <- sort(sample(1:144, 72, replace = FALSE))

O_vec_corr_50 <- O_vec
O_vec_corr_50[inds] <- O_vec_corr_50[inds] * (-1)

inds <- sort(sample(1:144, 72, replace = FALSE))

M_vec_corr_50 <- M_vec
M_vec_corr_50[inds] <- M_vec_corr_50[inds] * (-1)

## Output the original, corrupted image

image.plot(rbind(rotate(matrix(E_vec_corr_50, nrow = 12, ncol = 12)), 
                 rotate(matrix(H_vec_corr_50, nrow = 12, ncol = 12)),
                 rotate(matrix(T_vec_corr_50, nrow = 12, ncol = 12)),
                 rotate(matrix(O_vec_corr_50, nrow = 12, ncol = 12)),
                 rotate(matrix(M_vec_corr_50, nrow = 12, ncol = 12))),
           col = heat.colors(12))

## Prepare the 50% corrupted input

input_corr_50 <- cbind(E_vec_corr_50, H_vec_corr_50, T_vec_corr_50, O_vec_corr_50, M_vec_corr_50)

## Recall the 50% corrupted images from memory

E_50_recall <- matrix(mem_matrix %*% E_vec_corr_50, nrow = 12, ncol = 12)
H_50_recall <- matrix(mem_matrix %*% H_vec_corr_50, nrow = 12, ncol = 12)
T_50_recall <- matrix(mem_matrix %*% T_vec_corr_50, nrow = 12, ncol = 12)
O_50_recall <- matrix(mem_matrix %*% O_vec_corr_50, nrow = 12, ncol = 12)
M_50_recall <- matrix(mem_matrix %*% M_vec_corr_50, nrow = 12, ncol = 12)

## Rebuild the image

image.plot(rbind(rotate(E_50_recall), rotate(H_50_recall), rotate(T_50_recall), rotate(O_50_recall), rotate(M_50_recall)), col = heat.colors(12))





##### Problem c

## Call the err_corr function with 25% corruption

err_corr(input_corr_25, input, mu, n, tol)

## Output the original, corrupted image

image.plot(rbind(rotate(matrix(E_vec_corr_25, nrow = 12, ncol = 12)), 
                 rotate(matrix(H_vec_corr_25, nrow = 12, ncol = 12)),
                 rotate(matrix(T_vec_corr_25, nrow = 12, ncol = 12)),
                 rotate(matrix(O_vec_corr_25, nrow = 12, ncol = 12)),
                 rotate(matrix(M_vec_corr_25, nrow = 12, ncol = 12))),
           col = heat.colors(12))

## Output the original image

image.plot(rbind(rotate(E), rotate(H), rotate(T), rotate(O), rotate(M)), col = heat.colors(12))

## Recall the images from memory

E_recall_mem <- matrix(mem_matrix %*% E_vec, nrow = 12, ncol = 12)
H_recall_mem <- matrix(mem_matrix %*% H_vec, nrow = 12, ncol = 12)
T_recall_mem <- matrix(mem_matrix %*% T_vec, nrow = 12, ncol = 12)
O_recall_mem <- matrix(mem_matrix %*% O_vec, nrow = 12, ncol = 12)
M_recall_mem <- matrix(mem_matrix %*% M_vec, nrow = 12, ncol = 12)

## Rebuild the image

image.plot(rbind(rotate(E_recall_mem), rotate(H_recall_mem), rotate(T_recall_mem), rotate(O_recall_mem), rotate(M_recall_mem)), col = heat.colors(12))















