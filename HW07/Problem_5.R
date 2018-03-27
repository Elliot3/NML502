########## Problem 5 ##########





## Load in the necessary packages

library(ggplot2)

## Build the recall function

recall_func <- function(X, Cx, W, Cw) {
    
    Cy <- apply(X, 2, function(x) {
        
        d <- apply(x - W, 2, norm, type = "2")
        min_d <- min(d)
        min_ind <- which(d == min_d)
        
        if (length(min_ind) != 1) {
            
            min_ind <- min_ind[1]
            
        }
        
        Cw[min_ind]
        
    })
    
    accuracy <- sum((Cy - Cx) == 0) / length(Cx)
    
    return(list(accuracy, Cy))
    
}

## Build the function for testing recall

recall_test_func <- function(X, W, Cw) {
    
    Cy <- apply(X, 2, function(x) {
        
        d <- apply(x - W, 2, norm, type = "2")
        min_d <- min(d)
        min_ind <- which(d == min_d)
        
        if (length(min_ind) != 1) {
            
            min_ind <- min_ind[1]
            
        }
        
        Cw[min_ind]
        
    })
    
    accuracy <- NA
    
    return(list(accuracy, Cy))
    
    
}

## Build the plotting function

plot_func <- function(A, nrows, ncols, W, Cw) {
    
    data <- data.frame(
        
        factor = factor(A),
        row = rep(1:nrows, each = ncols),
        col = rep(1:ncols, times = nrows)
        
    )
    
    plot <- ggplot() +
        geom_rect(aes(xmin = col - 0.5, xmax = col + 0.5, ymin = -row - 0.5, ymax = 0.5 - row, fill = factor), 
                  data = data, alpha = 0.5) +
        xlim(0.5, ncols + 0.5) +
        ylim(-0.5 - nrows, -0.5) + 
        theme(axis.text = element_blank(), axis.ticks = element_blank()) +
        labs(fill = "Class Labels") +
        xlab("") +
        ylab("")
    
    proto_data <- data.frame(
        
        factor = Cw,
        row = t(W)[, 1],
        col = t(W)[, 2]
        
    )
    
    proto_data$factor <- as.factor(proto_data$factor)
    
    plot = plot + geom_point(aes(x = col, y = -row, color = factor), data = proto_data) + guides(color = FALSE)
    
    return(plot)
    
}



##### Part a #####



A = c(3, 1, 1, 1, 1, 2, 3, 3, 3,
      3, 3, 3, 3, 3, 2, 3, 3, 3,
      3, 3, 3, 3, 3, 1, 2, 2, 2,
      3, 1, 1, 1, 1, 1, 1, 1, 1,
      3, 1, 1, 1, 2, 1, 1, 1, 1,
      3, 1, 1, 2, 2, 2, 1, 1, 1,
      3, 1, 2, 2, 1, 3, 2, 2, 2,
      3, 3, 3, 3, 3, 3, 2, 2, 2,
      3, 3, 3, 3, 3, 3, 3, 3, 2)

nrows <- 9
ncols <- 9
ndim <- 2

# A <- matrix(A, nrow = nrows, ncol = ncols, byrow = FALSE)

N <- nrows * ncols
nP <- 60
nC <- 3
maxsteps <- 100000
tol <- 0.96
mu <- 0.5
LRsched <- matrix(
    c(0, 0.5, 15000, 0.25, 30000, 0.125, 45000, 0.05),
    ncol = 2, byrow = TRUE
)
Mfrsched <- c(1, seq(from = 5000, to = maxsteps, by = 5000))
prots_per_class <- round(nP / nC)
Cw <- rep(1:nC, each = prots_per_class)

X <- rbind(rep(1:nrows, each = ncols), rep(1:ncols, nrows))
Cx <- A

W <- matrix(runif(ndim * nP, min(X), max(X)), nrow = ndim, ncol = nP)

ler_step <- numeric()
step_acc <- numeric()
plot_container <- list()

for (i in 1:maxsteps) {
    
    mu <- tail(LRsched[i > LRsched[, 1], 2], 1)
    
    rand_ind <- sample(1:N, 1)
    
    d <- apply(W - X[, rand_ind], 2, norm, type = "2")
    min_d <- min(d)
    min_ind <- which(d == min_d)
    
    if (Cw[min_ind] == Cx[rand_ind]) {
        
        W[, min_ind] <- W[, min_ind] + mu * (X[, rand_ind] - W[, min_ind])
        
    } else {
        
        W[, min_ind] <- W[, min_ind] - mu * (X[, rand_ind] - W[, min_ind])
        
    }
    
    if (i %in% Mfrsched) {
        
        ler_step[length(ler_step) + 1] <- i
        
        recall <- recall_func(X, Cx, W, Cw)
        accuracy <- recall[[1]]
        step_acc[length(step_acc) + 1] <- accuracy
        plot <- plot_func(A = A, nrows = nrows, ncols = ncols, W = W, Cw = Cw)
        plot <- plot + labs(title = paste("Learning Step: ", i))
        
        plot_container[[length(plot_container) + 1]] <- plot
        
        if (accuracy > tol) {
            
            break
            
        }
        
    }
    
}

len <- length(plot_container)

plot_container[[len]]

ggplot() +
    geom_line(aes(x = ler_step, y = step_acc)) +
    geom_hline(aes(yintercept = tol), color = "red", linetype = "dashed") +
    labs(x = "Learning Step", y = "Classification Accuracy") +
    ggtitle("Learning History")



##### Part b #####



nrows <- 18
ncols <- 18
ndim <- 2

N <- nrows * ncols

X <- rbind(rep(1:nrows, each = ncols), rep(1:ncols, times = nrows)) / 2

recall <- recall_test_func(X, W, Cw)
Cy <- recall[[2]]

plot <- plot_func(Cy, nrows, ncols, W = W * 2, Cw = Cw)

plot + ggtitle("Test Recall")

data_1 <- data.frame(
    
    factor = factor(rep(factor(A), times = 4)),
    row = rep(c(1,3,5,7,9,11,13,15,17,2,4,6,8,10,12,14,16,18), times = 2, each = (nrows / 2)),
    col = c(rep(c(1,3,5,7,9,11,13,15,17), times = 18), rep(c(2,4,6,8,10,12,14,16,18), times = 18))
    
)

data_2 <- data.frame(
    
    factor = factor(Cy),
    row = rep(1:nrows, each = ncols),
    col = rep(1:ncols, times = nrows)
    
)

join_data <- inner_join(data_1, data_2, by = c('row', 'col'))
join_data$diff <- as.numeric(join_data$factor.x) - as.numeric(join_data$factor.y)

count_correct <- sum(join_data$diff == 0)
count_total <- dim(join_data)[1]
count_correct / count_total



ggplot() +
    geom_rect(aes(xmin = col - 0.5, xmax = col + 0.5, ymin = -row - 0.5, ymax = 0.5 - row, fill = factor.x),
              data = join_data, alpha = 0.5) +
    theme(axis.text = element_blank(), axis.ticks = element_blank()) +
    xlab("") +
    ylab("") +
    labs(fill = "Class Labels") +
    ggtitle("Final Training Results")

ggplot() +
    geom_rect(aes(xmin = col - 0.5, xmax = col + 0.5, ymin = -row - 0.5, ymax = 0.5 - row, fill = factor.y),
              data = join_data, alpha = 0.5) +
    theme(axis.text = element_blank(), axis.ticks = element_blank()) +
    xlab("") +
    ylab("") +
    labs(fill = "Class Labels") +
    ggtitle("Final Test Results")


