## Load in the iris training data

train_data <- read.table("~/Documents/Rice_University/Spring_2018/NML502/HW04_Part2/iris-train.txt", skip = 8)
train_attr <- train_data[c(TRUE, FALSE), ]
train_cats <- train_data[c(FALSE, TRUE), ][ ,2:4]

train_data_final <- cbind(train_attr, train_cats)

train_x <- transpose(train_data_final[, 1:4])
train_y <- transpose(train_data_final[, 5:7])

train_x <- matrix(as.numeric(unlist(train_x)), nrow = nrow(train_x))
train_y <- matrix(as.numeric(unlist(train_y)), nrow = nrow(train_y))

## Load in the iris test data

test_data <- read.table("~/Documents/Rice_University/Spring_2018/NML502/HW04_Part2/iris-test.txt", skip = 8)
test_attr <- test_data[c(TRUE, FALSE), ]
test_cats <- test_data[c(FALSE, TRUE), ][ ,2:4]

test_data_final <- cbind(test_attr, test_cats)

test_x <- transpose(test_data_final[, 1:4])
test_y <- transpose(test_data_final[, 5:7])

test_x <- matrix(as.numeric(unlist(test_x)), nrow = nrow(test_x))
test_y <- matrix(as.numeric(unlist(test_y)), nrow = nrow(test_y))
