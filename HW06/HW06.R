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

eigen_vec <- iris_pca$sdev
eigen_vals <- iris_pca$rotation

## Confirm we get the identity matrix

round(iris_pca$rotation %*% t(iris_pca$rotation))

##### Problem 2.2

