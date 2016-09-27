# Section 2
# Group C
#  Jennifer Chu
#  Samnang Tep
#  Ryan Quigley
# Problem 3

# a)
load("ps04p3.RData")
class(X)
n <- dim(X)
n[1]*n[2]
class(y)
length(y)

# b)
x.nums <- 1:dim(X)[2]
x.col.names <- paste("x", x.nums, sep = "")
colnames(X) <- x.col.names
colnames(X)

# c)
?solve
# b: a numeric or complex vector or matrix giving the right-hand side(s) of the linear system. If missing, b is taken to be an identity matrix and solve will return the inverse of a.

system.time({
	xt <- t(X)
	xt.x.1 <- xt %*% X
	xt.x.inv.1 <- solve(xt.x.1)
	xt.y.1 <- xt %*% y
	beta1 <- xt.x.inv.1 %*% xt.y.1
})
dim(beta1)

# d)
?crossprod

system.time({
	xt.x.2 <- crossprod(X)
	xt.x.inv.2 <- solve(xt.x.2)
	xt.y.2 <- crossprod(X, y)
	beta2 <- crossprod(xt.x.inv.2, xt.y.2)
})
dim(beta2)

# e)
system.time({
	xt.x.3 <- crossprod(X)
	xt.y.3 <- crossprod(X, y)
	beta3 <- solve(xt.x.3, xt.y.3)
})

# f)
# Can the following two lines be combined into one?
all.equal(beta1, beta2, tol = 1e-12)
all.equal(beta1, beta3, tol = 1e-12)

# g)

# h)
X.df <- data.frame(X)

system.time({
	x.t <- t(X.df)
	a <- x.t %*% X.df
	a.inv <- solve(a)
	xt.y1 <- x.t %*% y
	beta4 <- a.inv %*% xt.y
})
dim(beta4)

system.time({
	xt.x.5 <- crossprod(X.df)
	xt.y.5 <- crossprod(X.df, y)
	beta5 <- solve(xt.x.5, xt.y.5)
})

?matmult