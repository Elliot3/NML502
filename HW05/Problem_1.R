## Number of iterations

num_iter <- 10000

## Learning rate for each layer

ler_rate <- c(0.05, 0.05)

## Batch size

K <- 10

## Update the learning rate to match the batch size

ler_rate <- ler_rate

## Set the forget rate

alpha <- c(0.4, 0.4)

## Define the hyperbolic tangent function

trans_func <- function(x) {
    
    ((exp(1)^x) - (exp(1)^-x))/((exp(1)^x) + (exp(1)^-x))
    
}

## Define the derivative of the hyperbolic tangent function

der_trans_func <- function(x) {
    
    (1 - (trans_func(x)^2))
    
}

## Load in the iris training data
getwd()
#train_data <- read.table("/c/Users/Eugen/Documents/DocumentsMac/COMP502/NML502/HW04_Part2/iris-train.txt", skip = 8)
train_data <- read.table("iris-train.txt", skip = 8)
#test_data <- read.table("~/Documents/Rice_University/Spring_2018/NML502/HW04_Part2/iris-test.txt", skip = 8)
test_data <- read.table("iris-test.txt", skip = 8)






train_attr <- train_data[c(TRUE, FALSE), ]
train_cats <- train_data[c(FALSE, TRUE), ][ ,2:4]

train_data_final <- cbind(train_attr, train_cats)

x <- t(train_data_final[, 1:4])
y <- t(train_data_final[, 5:7])

x <- matrix(as.numeric(unlist(x)), nrow = nrow(x))
y <- matrix(as.numeric(unlist(y)), nrow = nrow(y))

## Load in the iris test data


test_attr <- test_data[c(TRUE, FALSE), ]
test_cats <- test_data[c(FALSE, TRUE), ][ ,2:4]

test_data_final <- cbind(test_attr, test_cats)

x_test <- t(test_data_final[, 1:4])
y_test <- t(test_data_final[, 5:7])

x_test <- matrix(as.numeric(unlist(x_test)), nrow = nrow(x_test))
y_test <- matrix(as.numeric(unlist(y_test)), nrow = nrow(y_test))

#combine train test


x_all <- cbind(x,x_test)
y_all <- cbind(y,y_test)

#get first 3-fold train data
num_fold <- 3


## Set up the network architecture

num_outputs <- c(4, 3, 3)
num_layers <- 2


## Train the network


#


train_index <- sample(1:dim(x_all)[2], dim(x_all)[2]*(num_fold-1)/num_fold, replace=F)
test_index <- setdiff(1:150,train_index)
x <- x_all[,train_index]
y <- y_all[,train_index]
x_test <- x_all[,test_index]
y_test <- y_all[,test_index]
results_a <- bp_learn_iris(num_iter, ler_rate, K, alpha, trans_func, der_trans_func, num_outputs, num_layers, x, y, 0.03, x_test, y_test)

train_index <- sample(1:dim(x_all)[2], dim(x_all)[2]*(num_fold-1)/num_fold, replace=F)
test_index <- setdiff(1:150,train_index)
x <- x_all[,train_index]
y <- y_all[,train_index]
x_test <- x_all[,test_index]
y_test <- y_all[,test_index]

results_b <- bp_learn_iris(num_iter, ler_rate, K, alpha, trans_func, der_trans_func, num_outputs, num_layers, x, y, 0.03, x_test, y_test)

train_index <- sample(1:dim(x_all)[2], dim(x_all)[2]*(num_fold-1)/num_fold, replace=F)
test_index <- setdiff(1:150,train_index)
x <- x_all[,train_index]
y <- y_all[,train_index]
x_test <- x_all[,test_index]
y_test <- y_all[,test_index]

results_c <- bp_learn_iris(num_iter, ler_rate, K, alpha, trans_func, der_trans_func, num_outputs, num_layers, x, y, 0.03, x_test, y_test)



length(results_a[[3]])
length(results_a[[4]])

ggplot() +
    geom_line(aes(x = results_a[[3]], y = results_a[[4]]), color = "blue") +
    geom_line(aes(x = results_a[[3]], y = results_a[[5]]), color = "red") +
    labs(x = "Learning Step", y = "Total Error", title = "Learning History", subtitle = "Blue - Training, Red - Testing")




ggplot() +
    geom_line(aes(x = results_a[[3]], y = results_a[[4]]), color = "blue") +
    geom_line(aes(x = results_a[[3]], y = results_a[[5]]), color = "blue",lty=2) +
    geom_line(aes(x = results_b[[3]], y = results_b[[4]]), color = "red") +
    geom_line(aes(x = results_b[[3]], y = results_b[[5]]), color = "red",lty=2) +
    geom_line(aes(x = results_c[[3]], y = results_c[[4]]), color = "green") +
    geom_line(aes(x = results_c[[3]], y = results_c[[5]]), color = "green",lty=2) +
    labs(x = "Learning Step", y = "Total Error", title = "Learning History", subtitle = "Solid lines - Training, Dashed lines - Test; red, blue, green - 3 folds")


print("confusion matrix 1. fold train")
results_a[[6]]
print("confusion matrix 1. fold test")
results_a[[7]]
print("confusion matrix 2. fold train")
results_b[[6]]
print("confusion matrix 2. fold test")
results_b[[7]]
print("confusion matrix 3. fold train")
results_c[[6]]
print("confusion matrix 3. fold test")
results_c[[7]]


print("classification accuracy 1. fold train")
results_a[[8]]
print("classification accuracy 1. fold test")
results_a[[9]]
print("classification accuracy 2. fold train")
results_b[[8]]
print("classification accuracy 2. fold test")
results_b[[9]]
print("classification accuracy 3. fold train")
results_c[[8]]
print("classification accuracy 3. fold test")
results_c[[9]]

print("classification accuracy average and standard deva training")
a <- c(results_a[[8]],results_b[[8]],results_c[[8]]) 
mean(a)
sd(a)

print("classification accuracy average and sd test")
b <- c(results_a[[9]],results_b[[9]],results_c[[9]]) 
mean(b)
sd(b)


library(lattice)
print("desired output (top three rows) vs actual output (bottom three rows)")
rgb.palette <- colorRampPalette(c("yellow", "blue"), space = "rgb")
levelplot(t(rbind(results_a[[11]],results_a[[10]])), main="", xlab="data", ylab="output", col.regions=rgb.palette(120), cuts=100, at=seq(0,1,0.01))
levelplot(t(rbind(results_b[[11]],results_b[[10]])), main="", xlab="data", ylab="output", col.regions=rgb.palette(120), cuts=100, at=seq(0,1,0.01))
levelplot(t(rbind(results_c[[11]],results_c[[10]])), main="", xlab="data", ylab="output", col.regions=rgb.palette(120), cuts=100, at=seq(0,1,0.01))

# Return the final network components

#weights <- train_results[[1]]
#biases <- train_results[[2]]
#ler_step <- train_results[[3]]
#errors_train <- train_results[[4]]
#errors_test <- train_results[[5]]
#6- y_train_out
#y_test_out




## Plot the RMSE over time

ggplot() +
    geom_line(aes(x = ler_step, y = errors_train), color = "blue") +
    geom_line(aes(x = ler_step, y = errors_test), color = "red") +
    labs(x = "Learning Step", y = "Total Error", title = "Learning History", subtitle = "Blue - Training, Red - Testing")




