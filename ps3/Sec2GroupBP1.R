### 1. ###
# v1
v1_1 <- 1:100
v1_2 <- c(1,-1)
v1 <- v1_1*v1_2
v1

# v2
v2 <- rep(1:99, rep(1:3, times = 33))
v2

# v3
x <- 1:100
v3 <- ifelse(x %% 3 == 0, cos(x), 1/sqrt(x))
v3


# v4
v4 <- rep("C", 100)
v4[v1 > 0] <- "A"
v4[(v1 < 0) & ((0 < v3) & (v3 < 0.138))] <- "B"
v4 <- factor(v4) # Should this be a factor or a character vector?
v4