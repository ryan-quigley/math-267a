# Section 2
# Group B 
#  Ian Dupzyk
#  Ryan Quigley
#  Sucharu Gupta
# Problem 1

# v1
v1_0 <- c(1:100)
v1_1 <- c(1,-1)
v1 <- v1_0 * v1_1

# v2
v2_0 <- c(1:99)
v2_1 <- rep(1:3, times = 33)
v2 <- rep(v2_0, v2_1)

# v3
v3_0 <- c(1:100)
v3 <- ifelse(v3_0 %% 3 == 0, cos(v3_0), 1/sqrt(v3_0))

# v4
v4_0 <- rep("C", each=100)
v4_1 <- replace(v4_0, v1>0, "A")
v4 <- replace(v4_1, ((v1<=0) & ((v3 >= 0) & (v3 <= 0.138))), "B")

