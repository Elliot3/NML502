file <- "~/Documents/Rice_University/Spring_2018/NML502/Lec4/characters.ascii.txt"
data <- read.table(file)
data <- as.matrix(data)

A <- matrix(data[1,], nrow = 7, ncol = 6, byrow = T)
B <- matrix(data[2,], nrow = 7, ncol = 6, byrow = T)
C <- matrix(data[3,], nrow = 7, ncol = 6, byrow = T)