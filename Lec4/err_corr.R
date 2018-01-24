x <- matrix(c(1,0,0,0,1,0,0,0,0,0,0,1), nrow = 4, ncol = 3, byrow = T)
y <- matrix(c(2,1,-2,-2,1,4,3,-2,2), nrow = 3, ncol = 3, byrow = T)
mu <- 0.1
n <- 100
tol <- 0.0001

# err_corr(x, y, mu, n, tol)

err_corr <- function(x, y, mu, n, tol) {
    
    x_dims <- dim(x)
    y_dims <- dim(y)
    
    mem_matrix <- matrix(0, nrow = y_dims[1], ncol = x_dims[1])
    
    for (i in 1:n) {
        
        j <- sample(1:x_dims[2])
        x <- x[, j]
        y <- y[, j]
        
        for (k in 1:x_dims[2]) {
            
            mem_matrix <- mem_matrix + mu * (y[,k] - mem_matrix * x[,k]) * x[,k]
            
        }
        
        if (max(svd(y - mem_matrix %*% x)$d) <= tol) {
            
            print("Gradient Search Terminated ===>>> ||y - mem_matrix*x|| <= tol")
            print("Number of Iterations = ")
            n*i
            break
            
        }
        
    }
    
    mem_matrix
    
}