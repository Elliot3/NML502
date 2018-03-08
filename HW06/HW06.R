##### Problem 2.1

## Load in the iris training data

train_data <- read.table("~/Documents/Rice_University/Spring_2018/NML502/HW04_Part2/iris-train.txt", skip = 8)
train_attr <- train_data[c(TRUE, FALSE), ]
train_attr$V1 <- as.numeric(as.character(train_attr$V1))
train_cats <- train_data[c(FALSE, TRUE), ][ ,2:4]

data <- cbind(train_attr, train_cats)
names(data) <- c("Sep_L", "Sep_W", "Pet_L", "Pet_W", "Setosa", "Versacolor", "Virginica")

## Scale the data

scaled_data <- apply(data[, 1:4], 2, function(x) {(x - mean(x)) / sd(x)})

## Perform the PCA

iris_pca <- prcomp(scaled_data)

eigen_vals <- iris_pca$sdev
eigen_vecs <- iris_pca$rotation

## Confirm we get the identity matrix

iris_pca$rotation %*% t(iris_pca$rotation)



##### Problem 2.2

n <- 50000
num_outputs <- c(4, 4)
num_layers <- 1
ler_rate <- 0.0001

weights <- list()

weights <- matrix(runif(num_outputs[num_layers] * num_outputs[num_layers + 1], min = -1, max = 1),
                  nrow = num_outputs[num_layers + 1],
                  ncol = num_outputs[num_layers])

y <- scaled_data

for (i in 1:n) {
    
    rand_ind <- sample(1:75, 1)
    
    x_output <- weights %*% matrix(y[rand_ind, ], ncol = 1)
    
    temp_mat <- matrix(0, nrow = 4, ncol = 4)
    
    for (j in 1:length(y[rand_ind, ])) {
        
        if (j == 1) {
            
            temp_mat[j, ] <- c(y[rand_ind, ][j] * (y[rand_ind, ][j] * weights[[j]]),
                               0, 0, 0)
            
        } else if (j == 2) {
            
            temp_mat[j, ] <- c(y[rand_ind, ][j] * y[rand_ind, ][j - 1] * weights[[j - 1]],
                               y[rand_ind, ][j] * y[rand_ind, ][j] * weights[[j]],
                               0, 0)
            
        } else if (j == 3) {
            
            temp_mat[j, ] <- c(y[rand_ind, ][j] * y[rand_ind, ][j - 2] * weights[[j - 2]],
                               y[rand_ind, ][j] * y[rand_ind, ][j - 1] * weights[[j - 1]],
                               y[rand_ind, ][j] * y[rand_ind, ][j] * weights[[j]],
                               0)
            
        } else if (j == 4) {
            
            temp_mat[j, ] <- c(y[rand_ind, ][j] * y[rand_ind, ][j - 3] * weights[[j - 3]],
                               y[rand_ind, ][j] * y[rand_ind, ][j - 2] * weights[[j - 2]],
                               y[rand_ind, ][j] * y[rand_ind, ][j - 1] * weights[[j - 1]],
                               y[rand_ind, ][j] * y[rand_ind, ][j] * weights[[j]])
            
        }
        
    }
    
    weights <- weights +
        ler_rate * (matrix(y[rand_ind, ], ncol = 1) %*% matrix(x_output, nrow = 1)) -
        ler_rate * temp_mat

}








