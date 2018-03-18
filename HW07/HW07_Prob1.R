## Set network parameters

num_iter <- 1000
ler_rate <- 0.01

## Load in the input vectors

x_1 <- runif(n = 2400)
x_2 <- runif(n = 2400)

X <- rbind(x_1, x_2)
rownames(X) <- NULL

## Get the size of a typical input

input_size <- length(X[, 1])

## Build the lattice matrix (list) of PEs

lattice_matrix <- list()

## Initialize the lattice cells

for (i in 1:100) {
    
    lattice_matrix[[i]] <- c(runif(1, min = -1, max = 1),
                             runif(1, min = -1, max = 1))
    
}

## Initialize the radius size and decay

radius <- sqrt(length(lattice_matrix)) / 2
radius_decay <- num_iter / log(radius)

## Learn the SOM

for (i in 1:num_iter) {
    
    ## Randomly select an input vector
    
    rand_ind <- sample(x = 1:dim(X)[2], size = 1)
    x_vec <- as.matrix(X[, rand_ind], ncol = 1)
    
    ## Find the Euclidean distance between x and all weights and locate winning neuron
    
    diff_matrix <- list()
    
    for (j in 1:length(lattice_matrix)) {
        
        diff_matrix[[j]] <- x_vec - as.matrix(lattice_matrix[[j]], ncol = 1)
        
    }
    
    dist_matrix <- list()
    
    for (j in 1:length(diff_matrix)) {
        
        dist_matrix[[j]] <- sqrt(sum(diff_matrix[[j]])^2)
        
        if (j == 1) {
            
            min_dist <- dist_matrix[[j]]
            min_ind <- j
            
        } else {
            
            if (dist_matrix[[j]] < min_dist) {
                
                min_ind <- j
                
            }
            
            min_dist <- min(min_dist, dist_matrix[[j]])
            
        }
        
    }
    
    ## Decay the SOM parameteres
    
    radius <- radius * exp(-i / radius_decay)
    ler_rate <- ler_rate * exp(-i / num_iter)
    
    ## Perform the updates to the SOM
    
    
    
    
    
}























