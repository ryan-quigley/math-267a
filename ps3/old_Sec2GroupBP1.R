### 1. ###
# v1
# Final Code: Ryan
v1.0 <- (1:100)
v1.1 <- c(1,-1)
v1 <- v1.0 * v1.1
v1

# v2
# Final Code: Ryan
v2.0 <- 1:99
v2.1 <- rep(1:3, times = 33)
v2 <- rep(v2.0, v2.1)
v2

# v3
# Final Code: Ryan
v3.0 <- 1:100
v3 <- ifelse(v3.0 %% 3 == 0, cos(v3.0), 1/sqrt(v3.0))
v3


# v4 
# Final Code: Ian
v4 <- rep("C", 100)
v4[v1 > 0] <- "A"
v4[(v1 < 0) & ((0 < v3) & (v3 < 0.138))] <- "B"
v4