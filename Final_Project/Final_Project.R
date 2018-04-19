## Timing

start.time <- Sys.time()

## Saving workspace image

# save(list = ls(.GlobalEnv), file = "~/Documents/Rice_University/Spring_2018/NML502/Final_Project/R_Env_Image")



########## Workspace Preparation ##########





options(scipen = 999)





##### Load Packages #####



library(dplyr)
library(plotly)



##### Construct the Functions #####



## Function to build the SOM lattice matrix

build_SOM <- function(input_size, matrix_dim) {
    
    ## Get the number of PEs
    
    num_PEs <- matrix_dim^2
    
    ## Build the list of weights
    
    SOM_weights <- runif(n = input_size * num_PEs, min = -0.5, max = 0.5)
    dim(SOM_weights) <- c(input_size, num_PEs)
    
    ## Return the results
    
    return(SOM_weights)
    
}

## Function to perform the SOM leaarning

learn_SOM <- function(input_data, SOM_lattice, num_iter, ler_rate, radius, matrix_dim, output_space) {
    
    ## Container for the current prototype lattice
    
    SOM_container <- list()
    
    ## Container for the indices
    
    SOM_indices <- list()
    
    ## Container for the parameters
    
    param_container <- list()
    
    ## Container for learning step, radius and learning rate and parameter bucket
    
    ler_steps <- numeric()
    radii <- numeric()
    ler_rates <- numeric()
    
    ## Build the list of weight indices
    
    SOM_indices[[1]] <- matrix(rep(1:matrix_dim, times = matrix_dim),
                               nrow = matrix_dim, ncol = matrix_dim)
    
    SOM_indices[[2]] <- matrix(rep(1:matrix_dim, times = matrix_dim),
                               byrow = T, nrow = matrix_dim, ncol = matrix_dim)
    
    ## Record the original ler_rate and radius
    
    init_ler_rate <- ler_rate
    init_radius <- radius
    
    ## Establish a decay constant
    
    decay_constant <- num_iter / log(init_radius)
    
    for (i in 1:num_iter) {
        
        ## Decay the learning rate
        
        ler_rate <- init_ler_rate * exp(-i / num_iter)
        
        ## Decay the radius
        
        radius <- init_radius * exp(-i / decay_constant)
        
        ## Add the parameters states to the container
        
        if ((i %% 5000 == 0) || (i == 1)) {
            
            ler_steps[length(ler_steps) + 1] <- i
            radii[length(radii) + 1] <- radius
            ler_rates[length(ler_rates) + 1] <- ler_rate
            
        }
        
        ## Randomly select an input vector
        
        rand_ind <- sample(x = 1:dim(input_data)[1], size = 1)
        input <- matrix(input_data[rand_ind, ], ncol = 1)
        
        ## Calculate the Euclidean distances
        
        list_diffs <- list()
        vec_dists <- numeric()
        
        for (j in 1:dim(SOM_lattice)[2]) {
            
            list_diffs[[j]] <- input - as.matrix(SOM_lattice[, j], 1)
            vec_dists[j] <- norm(list_diffs[[j]], type = "2")
            
        }
        
        ## Find the winning PE
        
        min_neuron <- min(vec_dists)
        min_list <- as.vector(which(vec_dists == min(vec_dists), arr.ind = TRUE))
        
        x_pt <- floor(min_list / matrix_dim) + 1
        y_pt <- min_list - ((x_pt - 1) * matrix_dim)
        
        if (y_pt == 0) {
            
            x_pt <- x_pt - 1
            y_pt <- 15
            
        }
        
        min_loc <- c(x_pt, y_pt)
        
        ## Calculate the Manhattan distance
        
        mat_diffs_y <- abs(SOM_indices[[1]] - min_loc[1])
        mat_diffs_x <- abs(SOM_indices[[2]] - min_loc[2])
        man_dist <- mat_diffs_x + mat_diffs_y
        neighbor_func <- as.vector(t(exp(-((man_dist)/(radius))^2)))
        
        ## Update the weights
        
        for (j in 1:dim(SOM_lattice)[2]) {
            
            SOM_lattice[, j] <- as.matrix(SOM_lattice[, j], ncol = 1) + ler_rate * (neighbor_func[j] * list_diffs[[j]])
            
        }
        
        ## Add lattice to the container
        
        if ((i == 1) || (i %in% seq(from = 0, to = num_iter, length.out = 6))) {

            recall_results <- recall_SOM(SOM_lattice, input_space, output_space)

            SOM_container[[length(SOM_container) + 1]] <- list(i, SOM_lattice, recall_results[[1]], recall_results[[2]])

        }
        
    }
    
    param_container[[1]] <- ler_steps
    param_container[[2]] <- radii
    param_container[[3]] <- ler_rates
    
    param_container <<- param_container
    
    return(SOM_container)
    
}

## Function to determine which PE an input maps to 

recall_SOM <- function(final_lattice, input_space, output_space) {
    
    ## Dimensions of the square lattice
    
    dim_lat <- sqrt(dim(final_lattice)[2])
    
    ## Container for the classes of each input
    
    class_container <- vector("list", length = dim_lat^2)
    
    ## Container for which PE the input maps to
    
    neuron_map <- matrix(0, ncol = dim_lat, nrow = dim_lat)
    colnames(neuron_map) <- 1:dim_lat
    rownames(neuron_map) <- 1:dim_lat
    
    for (i in 1:dim(input_space)[1]) {
        
        ## Extract the input vector 
        
        input <- as.matrix(input_space[i, ], ncol = 1)
        
        ## Calculate the Euclidean distance
        
        list_diffs <- list()
        vec_dists <- numeric()
        
        for (j in 1:dim(final_lattice)[2]) {
            
            list_diffs[[j]] <- input - as.matrix(final_lattice[, j], ncol = 1)
            vec_dists[j] <- norm(list_diffs[[j]], type = "2")
            
        }
        
        ## Find the winning PE
        
        min_neuron <- min(vec_dists)
        min_list <- as.vector(which(vec_dists == min(vec_dists), arr.ind = TRUE))
        
        x_pt <- floor(min_list / matrix_dim) + 1
        y_pt <- min_list - ((x_pt - 1) * matrix_dim)
        
        if (y_pt == 0) {
            
            x_pt <- x_pt - 1
            y_pt <- 15
            
        }
        
        min_loc <- c(x_pt, y_pt)
        
        ## Add the location to the neuron map
        
        if (neuron_map[x_pt, y_pt] == 0) {
            
            neuron_map[x_pt, y_pt] <- 1
            
        } else {
            
            neuron_map[x_pt, y_pt] <- neuron_map[x_pt, y_pt] + 1
            
        }
        
        ## Add the class to the class list
        
        class_container[[min_list]][length(class_container[[min_list]]) + 1] <- output_space[i]
        
    }
    
    ## Get the winning class for each neuron
    
    win_class <- character()
    
    for (i in 1:length(class_container)) {
        
        if (!is.null(class_container[[i]])) {
        
            win_class[i] <- names(sort(table(class_container[[i]]), decreasing=TRUE)[1])
            
        } else {
            
            win_class[i] <- 0
            
        }
        
    }
    
    win_mat <- matrix(win_class, nrow = matrix_dim, ncol = matrix_dim)
    win_mat <- t(win_mat)
    
    temp_vec <- as.vector(neuron_map)
    temp_mat <- matrix(temp_vec, nrow = matrix_dim, ncol = matrix_dim)
    temp_mat <- apply(temp_mat, 2, rev)
    
    recall_results <- list()
    
    recall_results[[1]] <- win_class
    recall_results[[2]] <- temp_mat
    
    return(recall_results)
    
}

## Function to build the squares of the mU-matrix

plot_square <- function(start, width, height, col) {
    
    p <- start
    
    polygon(
        
        x = c(p[1], p[1] + width, p[1] + width, p[1]), 
        y = c(p[2], p[2], p[2] + height, p[2] + height), col = col, border = NA
        
    )
    
}

## Function to plot the prototypes for the SOM lattice

SOM_prototype <- function(p, border, cols) {
    
    cols <- c(cols[2:5], cols[1], cols[6:9])
    
    plot_square(p, border, border, col = cols[1])
    plot_square(c(p[1] + border, p[2]), 1 - 2 * border, border, col = cols[2])
    plot_square(c(p[1] + 1 - border, p[2]), border, border, col = cols[3])
    
    plot_square(c(p[1], p[2] + border), border, 1 - 2 * border, col = cols[4])
    plot_square(c(p[1] + border, p[2] + border), 1 - 2 * border, 1 - 2 * border, col = cols[5])
    plot_square(c(p[1] + 1 - border, p[2] + border), border, 1 - 2 * border, col = cols[6])
    
    plot_square(c(p[1], p[2] + 1 - border), border, border, col = cols[7])
    plot_square(c(p[1] + border, p[2] + 1 - border), 1 - 2 * border, border, col = cols[8])
    plot_square(c(p[1] + 1 - border, p[2] + 1 - border), border, border, col = cols[9])
    
}

## Function to plot the whole mU-matrix

plot_mU_matrix <- function(W, width, height, classes, density,
                           colornames = c('red', 'lightgoldenrod3', 'blue', 'green4', 'orange', 'purple', 'magenta', 'turquoise', 'indianred4', 'aquamarine4'),
                           border = 0.05, border_max_scale = 3, xlab = '', ylab = '', ...) {
    
    index <- seq_len(ncol(W))
    dim(index) <- c(width, height)
    
    palette <- colorRampPalette(c('black', 'white'))
    cols <- palette(1000)
    ncols <- length(cols)
    
    classes = as.numeric(factor(classes))
    classes[is.na(classes)] <- 1
    clscolors <- lapply(sort(unique(classes)), function(i) {
        
        palette = colorRampPalette(c('black', colornames[i]))(100)
        
    })
    
    density <- ceiling(density / max(density) * 99)
    
    plot(0:height, c(rep(0, height), width), type='n', asp = 1, axes = F, xlab = xlab, ylab = ylab, ...)
    
    index <- t(index)
    
    junk <- rbind(
        c(-1, -1),
        c(0, -1),
        c(1, -1),
        c(-1, 0),
        c(1, 0),
        c(-1, 1),
        c(0, 1),
        c(1, 1)
    )
    
    for(i in 1:width) {
        
        for(j in 1:height) {
            
            ind = index[i, j]
            apply(junk, 1, function(shift) {
                
                tryCatch({
                    
                    ind2 = index[i + shift[2], j + shift[1]]
                    d = norm(W[ind] - W[ind2], type = '2')
                    return(d)
                    
                }, error = function(e){
                    
                    print(c(i, j))
                    return(0)
                    
                })
                
            }) -> dist
            dist <- floor(dist / border_max_scale * ncols) + 1
            dist[dist > ncols] <- ncols
            col <- clscolors[[classes[ind]]][density[ind]]
            SOM_prototype(c(j - 1, i - 1), border, c(col, cols[dist]))
        }
        
    }
    
}



##### Load the Data #####



## Import the data

data <- read.csv("~/Documents/Rice_University/Spring_2018/NML502/Final_Project/recipeData.csv")

## Select my beer styles and variables of interest

selected_styles <- c(7, 10, 134, 9, 4, 30, 86, 12, 92, 6)

data_final <- data %>%
    select(StyleID, Size.L., OG, FG, ABV, IBU, Color, BoilSize, BoilTime, BoilGravity, Efficiency) %>%
    filter(StyleID %in% selected_styles)

## Remove rows with NAs

data_final[data_final == "N/A"] <- NA
data_final <- data_final[complete.cases(data_final), ]

data_final$StyleID <- as.factor(data_final$StyleID)
data_final$BoilGravity <- as.numeric(data_final$BoilGravity)

data_final_orig <- data_final

## Eugen's data normalization method

for(i in 2:11) {
    
    data_final[,i] <- (data_final_orig[,i]-(mean(data_final_orig[,i])-2*sd(data_final_orig[,i])))/((mean(data_final_orig[,i])+2*sd(data_final_orig[,i]))-(mean(data_final_orig[,i])-2*sd(data_final_orig[,i])))
    data_final[data_final[,i]>1,i]=1
    data_final[data_final[,i]<0,i]=0
    
}

for(i in 2:11) {
    
    data_final[,i] <- (data_final[,i]-(mean(data_final[,i])-2*sd(data_final[,i])))/((mean(data_final[,i])+2*sd(data_final[,i]))-(mean(data_final[,i])-2*sd(data_final[,i])))
    data_final[data_final[,i]>1,i]=1
    data_final[data_final[,i]<0,i]=0
    
}

## Separate the input space from the labels

output_space <- matrix(as.numeric(unlist(data_final$StyleID)), nrow = length(data_final$StyleID))
input_space <- matrix(as.numeric(unlist(data_final[, 2:dim(data_final)[2]])), nrow = nrow(data_final[, 2:dim(data_final)[2]]))

## Define some data details

input_size <- dim(input_space)[2]
matrix_dim <- 15



##### Build the Network #####



## Set some network parameters

ler_rate <- 0.3
num_iter <- 1000000
radius <- matrix_dim / 2

## Build the weight matrix

SOM_lattice <- build_SOM(input_size, matrix_dim)




########## Perform Analysis ##########



## Learn the network

learn_results <- learn_SOM(input_data = input_space, SOM_lattice, num_iter, ler_rate, radius, matrix_dim, output_space)

## Extract the final lattice of prototypes

final_lattice <- learn_results[[length(learn_results)]][[2]]

## Recall Results

recall_results <- recall_SOM(final_lattice, input_space, output_space)

win_class <- recall_results[[1]]
temp_mat <- recall_results[[2]]



########## Plot Results ##########



##### Plot Decays #####



plot(x = param_container[[1]], y = param_container[[2]], type = "l",
     xlab = "Learning Step",
     ylab = "Neighborhood Radius",
     main = "Neighborhood Radius Decay")

plot(x = param_container[[1]], y = param_container[[3]], type = "l",
     xlab = "Learning Step",
     ylab = "Learning Rate",
     main = "Learning Rate Decay")



##### Make the Density Plots #####



s <- as.character(1:matrix_dim)

par(mfrow = c(1,1), mar = c(5.1, 4.1, 4.1, 2.1))

## First Plot

plot_ly(z = learn_results[[1]][[4]], x = ~s, y = ~s, colors = colorRamp(c("white", "black")), type = "heatmap") %>%
    layout(title = paste0("PE Density Map at Step: ", learn_results[[1]][[1]]), xaxis = list(title = "PE X Coordinate"), yaxis = list(title = "PE Y Coordinate"))

## Second Plot

plot_ly(z = learn_results[[2]][[4]], x = ~s, y = ~s, colors = colorRamp(c("white", "black")), type = "heatmap") %>%
    layout(title = paste0("PE Density Map at Step: ", learn_results[[2]][[1]]), xaxis = list(title = "PE X Coordinate"), yaxis = list(title = "PE Y Coordinate"))

## Third Plot

plot_ly(z = learn_results[[3]][[4]], x = ~s, y = ~s, colors = colorRamp(c("white", "black")), type = "heatmap") %>%
    layout(title = paste0("PE Density Map at Step: ", learn_results[[3]][[1]]), xaxis = list(title = "PE X Coordinate"), yaxis = list(title = "PE Y Coordinate"))

## Fourth Plot

plot_ly(z = learn_results[[4]][[4]], x = ~s, y = ~s, colors = colorRamp(c("white", "black")), type = "heatmap") %>%
    layout(title = paste0("PE Density Map at Step: ", learn_results[[4]][[1]]), xaxis = list(title = "PE X Coordinate"), yaxis = list(title = "PE Y Coordinate"))

## Fifth Plot

plot_ly(z = learn_results[[5]][[4]], x = ~s, y = ~s, colors = colorRamp(c("white", "black")), type = "heatmap") %>%
    layout(title = paste0("PE Density Map at Step: ", learn_results[[5]][[1]]), xaxis = list(title = "PE X Coordinate"), yaxis = list(title = "PE Y Coordinate"))

## Sixth Plot

plot_ly(z = learn_results[[6]][[4]], x = ~s, y = ~s, colors = colorRamp(c("white", "black")), type = "heatmap") %>%
    layout(title = paste0("PE Density Map at Step: ", learn_results[[6]][[1]]), xaxis = list(title = "PE X Coordinate"), yaxis = list(title = "PE Y Coordinate"))



##### Make the Vector Plots



par(mfrow = c(matrix_dim, matrix_dim))
par(mar = c(0, 0, 0, 0))



## First Plot

scaled_lattice <- learn_results[[1]][[2]]
win_class <- learn_results[[1]][[3]]

plot_col <- character()

for (i in 1:length(win_class)) {
    
    if (win_class[i] == "7") {
        
        plot_col[i] <- "red"
        
    } else if (win_class[i] == "10") {
        
        plot_col[i] <- "lightgoldenrod3"
        
    } else if (win_class[i] == "134") {
        
        plot_col[i] <- "blue"
        
    } else if (win_class[i] == "9") {
        
        plot_col[i] <- "green4"
        
    } else if (win_class[i] == "4") {
        
        plot_col[i] <- "orange"
        
    } else if (win_class[i] == "30") {
        
        plot_col[i] <- "purple"
        
    } else if (win_class[i] == "86") {
        
        plot_col[i] <- "magenta"
        
    } else if (win_class[i] == "12") {
        
        plot_col[i] <- "turquoise"
        
    } else if (win_class[i] == "92") {
        
        plot_col[i] <- "indianred4"
        
    } else if (win_class[i] == "6") {
        
        plot_col[i] <- "aquamarine4"
        
    } else {
        
        plot_col[i] <- "black"
        
    }
    
}

for (i in 1:dim(scaled_lattice)[2]) {
    
    y_vals <- c(scaled_lattice[, i])
    
    plot(x = 1:input_size,
         y = y_vals,
         type = "l",
         xaxt = "n",
         yaxt = "n",
         ann = FALSE,
         col = plot_col[i]
         #xlim = c(-3, 3),
         #ylim = c(-3, 3)
    )
    
}

## Second Plot

scaled_lattice <- learn_results[[2]][[2]]
win_class <- learn_results[[2]][[3]]

plot_col <- character()

for (i in 1:length(win_class)) {
    
    if (win_class[i] == "7") {
        
        plot_col[i] <- "red"
        
    } else if (win_class[i] == "10") {
        
        plot_col[i] <- "lightgoldenrod3"
        
    } else if (win_class[i] == "134") {
        
        plot_col[i] <- "blue"
        
    } else if (win_class[i] == "9") {
        
        plot_col[i] <- "green4"
        
    } else if (win_class[i] == "4") {
        
        plot_col[i] <- "orange"
        
    } else if (win_class[i] == "30") {
        
        plot_col[i] <- "purple"
        
    } else if (win_class[i] == "86") {
        
        plot_col[i] <- "magenta"
        
    } else if (win_class[i] == "12") {
        
        plot_col[i] <- "turquoise"
        
    } else if (win_class[i] == "92") {
        
        plot_col[i] <- "indianred4"
        
    } else if (win_class[i] == "6") {
        
        plot_col[i] <- "aquamarine4"
        
    } else {
        
        plot_col[i] <- "black"
        
    }
    
}

for (i in 1:dim(scaled_lattice)[2]) {
    
    y_vals <- c(scaled_lattice[, i])
    
    plot(x = 1:input_size,
         y = y_vals,
         type = "l",
         xaxt = "n",
         yaxt = "n",
         ann = FALSE,
         col = plot_col[i]
         #xlim = c(-3, 3),
         #ylim = c(-3, 3)
    )
    
}

## Third Plot

scaled_lattice <- learn_results[[3]][[2]]
win_class <- learn_results[[3]][[3]]

plot_col <- character()

for (i in 1:length(win_class)) {
    
    if (win_class[i] == "7") {
        
        plot_col[i] <- "red"
        
    } else if (win_class[i] == "10") {
        
        plot_col[i] <- "lightgoldenrod3"
        
    } else if (win_class[i] == "134") {
        
        plot_col[i] <- "blue"
        
    } else if (win_class[i] == "9") {
        
        plot_col[i] <- "green4"
        
    } else if (win_class[i] == "4") {
        
        plot_col[i] <- "orange"
        
    } else if (win_class[i] == "30") {
        
        plot_col[i] <- "purple"
        
    } else if (win_class[i] == "86") {
        
        plot_col[i] <- "magenta"
        
    } else if (win_class[i] == "12") {
        
        plot_col[i] <- "turquoise"
        
    } else if (win_class[i] == "92") {
        
        plot_col[i] <- "indianred4"
        
    } else if (win_class[i] == "6") {
        
        plot_col[i] <- "aquamarine4"
        
    } else {
        
        plot_col[i] <- "black"
        
    }
    
}

for (i in 1:dim(scaled_lattice)[2]) {
    
    y_vals <- c(scaled_lattice[, i])
    
    plot(x = 1:input_size,
         y = y_vals,
         type = "l",
         xaxt = "n",
         yaxt = "n",
         ann = FALSE,
         col = plot_col[i]
         #xlim = c(-3, 3),
         #ylim = c(-3, 3)
    )
    
}

## Fourth Plot

scaled_lattice <- learn_results[[4]][[2]]
win_class <- learn_results[[4]][[3]]

plot_col <- character()

for (i in 1:length(win_class)) {
    
    if (win_class[i] == "7") {
        
        plot_col[i] <- "red"
        
    } else if (win_class[i] == "10") {
        
        plot_col[i] <- "lightgoldenrod3"
        
    } else if (win_class[i] == "134") {
        
        plot_col[i] <- "blue"
        
    } else if (win_class[i] == "9") {
        
        plot_col[i] <- "green4"
        
    } else if (win_class[i] == "4") {
        
        plot_col[i] <- "orange"
        
    } else if (win_class[i] == "30") {
        
        plot_col[i] <- "purple"
        
    } else if (win_class[i] == "86") {
        
        plot_col[i] <- "magenta"
        
    } else if (win_class[i] == "12") {
        
        plot_col[i] <- "turquoise"
        
    } else if (win_class[i] == "92") {
        
        plot_col[i] <- "indianred4"
        
    } else if (win_class[i] == "6") {
        
        plot_col[i] <- "aquamarine4"
        
    } else {
        
        plot_col[i] <- "black"
        
    }
    
}

for (i in 1:dim(scaled_lattice)[2]) {
    
    y_vals <- c(scaled_lattice[, i])
    
    plot(x = 1:input_size,
         y = y_vals,
         type = "l",
         xaxt = "n",
         yaxt = "n",
         ann = FALSE,
         col = plot_col[i]
         #xlim = c(-3, 3),
         #ylim = c(-3, 3)
    )
    
}

## Fifth Plot

scaled_lattice <- learn_results[[5]][[2]]
win_class <- learn_results[[5]][[3]]

plot_col <- character()

for (i in 1:length(win_class)) {
    
    if (win_class[i] == "7") {
        
        plot_col[i] <- "red"
        
    } else if (win_class[i] == "10") {
        
        plot_col[i] <- "lightgoldenrod3"
        
    } else if (win_class[i] == "134") {
        
        plot_col[i] <- "blue"
        
    } else if (win_class[i] == "9") {
        
        plot_col[i] <- "green4"
        
    } else if (win_class[i] == "4") {
        
        plot_col[i] <- "orange"
        
    } else if (win_class[i] == "30") {
        
        plot_col[i] <- "purple"
        
    } else if (win_class[i] == "86") {
        
        plot_col[i] <- "magenta"
        
    } else if (win_class[i] == "12") {
        
        plot_col[i] <- "turquoise"
        
    } else if (win_class[i] == "92") {
        
        plot_col[i] <- "indianred4"
        
    } else if (win_class[i] == "6") {
        
        plot_col[i] <- "aquamarine4"
        
    } else {
        
        plot_col[i] <- "black"
        
    }
    
}

for (i in 1:dim(scaled_lattice)[2]) {
    
    y_vals <- c(scaled_lattice[, i])
    
    plot(x = 1:input_size,
         y = y_vals,
         type = "l",
         xaxt = "n",
         yaxt = "n",
         ann = FALSE,
         col = plot_col[i]
         #xlim = c(-3, 3),
         #ylim = c(-3, 3)
    )
    
}

## Sixth Plot

scaled_lattice <- learn_results[[6]][[2]]
win_class <- learn_results[[6]][[3]]

plot_col <- character()

for (i in 1:length(win_class)) {
    
    if (win_class[i] == "7") {
        
        plot_col[i] <- "red"
        
    } else if (win_class[i] == "10") {
        
        plot_col[i] <- "lightgoldenrod3"
        
    } else if (win_class[i] == "134") {
        
        plot_col[i] <- "blue"
        
    } else if (win_class[i] == "9") {
        
        plot_col[i] <- "green4"
        
    } else if (win_class[i] == "4") {
        
        plot_col[i] <- "orange"
        
    } else if (win_class[i] == "30") {
        
        plot_col[i] <- "purple"
        
    } else if (win_class[i] == "86") {
        
        plot_col[i] <- "magenta"
        
    } else if (win_class[i] == "12") {
        
        plot_col[i] <- "turquoise"
        
    } else if (win_class[i] == "92") {
        
        plot_col[i] <- "indianred4"
        
    } else if (win_class[i] == "6") {
        
        plot_col[i] <- "aquamarine4"
        
    } else {
        
        plot_col[i] <- "black"
        
    }
    
}

for (i in 1:dim(scaled_lattice)[2]) {
    
    y_vals <- c(scaled_lattice[, i])
    
    plot(x = 1:input_size,
         y = y_vals,
         type = "l",
         xaxt = "n",
         yaxt = "n",
         ann = FALSE,
         col = plot_col[i]
         #xlim = c(-3, 3),
         #ylim = c(-3, 3)
    )
    
}





########## TESTING ##########





# plot_mU_matrix(final_lattice, width = 15, height = 15, classes = c(rep(1, 225)), density = c(round(runif(225) * 10)))



## Timing

end.time <- Sys.time()

end.time - start.time







