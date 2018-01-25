

### Sample data from Lecture

# x <- matrix(c(1,0,0,0,1,0,0,0,0,0,0,1), nrow = 4, ncol = 3, byrow = T)
# y <- matrix(c(2,1,-2,-2,1,4,3,-2,2), nrow = 3, ncol = 3, byrow = T)
# mu <- 0.2
# n <- 1000
# tol <- 0.0001

### Command to run the function with the sample data

# err_corr(x, y, mu, n, tol)

err_corr <- function(x, y, mu, n, tol) {
    
    x_orig <- x
    y_orig <- y
    
    x_dims <- dim(x)
    y_dims <- dim(y)
    
    mem_matrix <- matrix(0, nrow = y_dims[1], ncol = x_dims[1])
    
    for (i in 1:n) {
        
        j <- sample(1:x_dims[2])
        x <- x[, j]
        y <- y[, j]
        
        for (k in 1:x_dims[2]) {
            
            mem_matrix <- mem_matrix + mu * (y[,k] - mem_matrix %*% x[,k]) %*% x[,k]
            
        }
        
        if (norm((y - mem_matrix %*% x), "2") <= tol) {
            
            print("Gradient Search Terminated ===>>> ||y - mem_matrix*x|| <= tol")
            print(paste0("Number of Iterations = ", n * k))
            break
            
        }
        
    }
    
    cat("\n")
    print("The Memory Matrix:")
    print(mem_matrix)
    cat("\n")
    print("Recall:")
    cat("\n")
    print("The Input Matrix:")
    print(x_orig)
    cat("\n")
    print("The desired output:")
    print(y_orig)
    cat("\n")
    print("Confirm: Memory Matrix * Input Matrix:")
    print(mem_matrix %*% x_orig)
    
}