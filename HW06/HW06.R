## Load in the iris training data

train_data <- read.table("~/Documents/Rice_University/Spring_2018/NML502/HW04_Part2/iris-train.txt", skip = 8)
train_attr <- train_data[c(TRUE, FALSE), ]
train_cats <- train_data[c(FALSE, TRUE), ][ ,2:4]

data <- cbind(train_attr, train_cats)
names(data) <- c("Sep_L", "Sep_W", "Pet_L", "Pet_W", "Setosa", "Versacolor", "Virginica")
