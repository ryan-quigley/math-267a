# Section 2
# Group C
#  Jennifer Chu
#  Samnang Tep
#  Ryan Quigley
# Problem 3

# a)
load("ps04p3.RData")
class(X)
length(X)
class(y)
length(y)

# b)
x.nums <- 1:ncol(X)
colnames(X) <- paste("x", x.nums, sep = "")

# c)
system.time({
	xt <- t(X)
	xt.x.1 <- xt %*% X
	xt.x.inv.1 <- solve(xt.x.1)
	xt.y.1 <- xt %*% y
	beta1 <- xt.x.inv.1 %*% xt.y.1
})

# d)
system.time({
	xt.x.2 <- crossprod(X)
	xt.x.inv.2 <- solve(xt.x.2)
	xt.y.2 <- crossprod(X, y)
	beta2 <- crossprod(xt.x.inv.2, xt.y.2)
})

# e)
system.time({
	xt.x.3 <- crossprod(X)
	xt.y.3 <- crossprod(X, y)
	beta3 <- solve(xt.x.3, xt.y.3)
})

# f)
all.equal(beta1, beta2, tol = 1e-12)
all.equal(beta1, beta3, tol = 1e-12)
all.equal(beta2, beta3, tol = 1e-12)


# h)
X.df <- data.frame(X)
system.time({
	xt <- t(X.df)
	xt.x.4 <- xt %*% X.df
	xt.x.inv.4 <- solve(a)
	xt.y.4 <- xt %*% y
	beta4 <- xt.x.inv.4 %*% xt.y
})