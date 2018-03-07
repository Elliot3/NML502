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

n <- 500
num_outputs <- c(4, 4)
num_layers <- 1
ler_rate <- 0.0001

weights <- list()

weights <- matrix(runif(num_outputs[num_layers] * num_outputs[num_layers + 1]),
                  nrow = num_outputs[num_layers + 1],
                  ncol = num_outputs[num_layers])

y <- scaled_data

temp_cell <- integer()

for (i in 1:n) {
    
    rand_ind <- sample(1:75, 1)
    
    x_output <- t(weights) %*% y[rand_ind, ]
    
    for (j in 1:length(y[rand_ind, ])) {
            
            temp_cell[j + 1,] <- temp_cell[j,] + y[rand_ind, ][[j]] * t(weights[j,])
        
    }
    
    weights <- weights + (ler_rate * y[rand_ind, ] %*% x_output) - (ler_rate * (y[rand_ind, ] %*% temp_cell))
    
}
