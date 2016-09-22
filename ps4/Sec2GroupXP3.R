# Section 2
# Group X
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
	x.t <- t(X)
	a <- x.t %*% X
	a.inv <- solve(a)
	xt.y1 <- x.t %*% y
	beta1 <- a.inv %*% xt.y
})
dim(beta1)

# d)
?crossprod

system.time({
	b <- crossprod(X)
	b.inv <- solve(b)
	xt.y2 <- crossprod(X, y)
	beta2 <- crossprod(b.inv, xt.y)
})
dim(beta2)

# e)
system.time({
	xt.x <- crossprod(X)
	xt.y3 <- crossprod(X, y)
	beta3 <- solve(xt.x, xt.y3)
})

# f)
# Can the following two lines be combined into one?
all.equal(beta1, beta2, tol = 1e-12)
all.equal(beta1, beta3, tol = 1e-12)

# g)

# h)
X.df <- data.frame(X)

system.time({
	x.t <- t(X)
	a <- x.t %*% X
	a.inv <- solve(a)
	xt.y1 <- x.t %*% y
	beta1 <- a.inv %*% xt.y
})
dim(beta1)