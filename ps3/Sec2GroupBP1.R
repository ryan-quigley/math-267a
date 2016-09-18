### 1. ###
# v1
v1_1 <- 1:100
v1_2 <- c(1,-1)
v1 <- v1_1*v1_2

# v2
v2 <- ep(1:99, rep(1:3, times = 33))

# v3
x <- 1:100
v3 <- ifelse(x %% 3 == 0, cos(x), 1/sqrt(x))
# check
v3[x %% 3 == 0] == cos(x[x %% 3 == 0]) # PASSED
v3[x %% 3 != 0] == 1/sqrt(x[x %% 3 != 0]) # PASSED

# v4
length(v1) == length(v3)
v4 <- rep(100, "C")
v4[v1 > 0] <- "A"
v4[(v1 < 0) & ((0 < v3) & (v3 < 0.138))] <- "B"
v4 <- factor(v4)
############################################
# DR. LEE: Should this be a factor or is a character vector ok
############################################