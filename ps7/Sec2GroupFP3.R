# Section 2
# Group F
#  Aran Lee
#  Tianyang Chen
#  Ryan Quigley
# Problem 3


# a)

epanechnikov <- function(a, x = 0, h = 1) {
	# a validation
	if (!is.numeric(x)) {
		stop("x must be numeric.")
	}
	# x validation
	if (!is.numeric(a)) {
		stop("a must be numeric.")
	}
	# h validation
	if (h <= 0) {
		stop("Bandwith (h) must be greater than 0.")
	}
	
	# Kernel computation
	if (length(x) > 1) {
		x.mat <- rep(x, each = length(a))
		Kh <- numeric(length(x.mat))
		u <- (a - x.mat)/h
		u.in <- u[abs(u) < 1]
		Kh.in <- (1/h)*(3/4)*(1 - u.in^2)
		Kh[abs(u) < 1] <- Kh.in
		dim(Kh) <- c(length(a), length(x))
	} else {
		Kh <- numeric(length(a))
		u <- (a - x)/h
		u.in <- u[abs(u) < 1]
		Kh.in <- (1/h)*(3/4)*(1 - u.in^2)
		Kh[abs(u) < 1] <- Kh.in
	}
	Kh
}



# b)
curve(epanechnikov(a, x = 0, h = 1), from = -3, to = 3, 
	xname = "a", xlim = c(-3,3), ylim = c(0,2), axes = FALSE, ann = FALSE, col = "violetred")
curve(epanechnikov(a, x = 0, h = 0.5), from = -3, to = 3, xname = "a", add = TRUE, col = "slateblue2")
curve(epanechnikov(a, x = 0, h = 2), from = -3, to = 3, xname = "a", add = TRUE, col = "gold")
curve(epanechnikov(a, x = 1, h = 0.75), from = -3, to = 3, xname = "a", add = TRUE, col = "springgreen")
axis(1, at = seq.int(-3, 3), labels = seq.int(-3, 3))
axis(2, at = seq.int(0, 2, by = 0.5), seq.int(0, 2, by = 0.5))
expr1 <- expression(K[1](a))
expr2 <- expression(K[0.5](a))
expr3 <- expression(K[2](a))
expr4 <- expression(K[0.75](a - 1))
legend(1, 2, legend = c(expr1, expr2, expr3, expr4), 
	bty = "n", lty = 1, col = c("violetred", "slateblue2", "gold", "springgreen"))
title(xlab = "a", ylab = "Density")



# c) function should return matrix
x <- c(1, 1.2, 1.5, 2.8, 3)
a.j <- seq.int(0, 4, length.out = 500)
h = 0.75
sand.piles <- (1/5)*epanechnikov(a.j, x = x, h = h)
m <- max(sand.piles)

plot(x, rep(0, 5), type = "n", xlim = c(0,4), ylim = c(0, 0.6), axes = FALSE, ann = FALSE)
axis(1, at = seq.int(0, 4), labels = seq.int(0, 4))
axis(2, at = seq.int(0, 0.6, by = 0.1), seq.int(0, 0.6, by = 0.1))
lines(a.j, sand.piles[, 1], col = "slateblue2", lty = 2)
lines(rep(x[1], 2), c(0, m), col = "grey50", lty = 3)
lines(a.j, sand.piles[, 2], col = "slateblue2", lty = 2)
lines(rep(x[2], 2), c(0, m), col = "grey50", lty = 3)
lines(a.j, sand.piles[, 3], col = "slateblue2", lty = 2)
lines(rep(x[3], 2), c(0, m), col = "grey50", lty = 3)
lines(a.j, sand.piles[, 4], col = "slateblue2", lty = 2)
lines(rep(x[4], 2), c(0, m), col = "grey50", lty = 3)
lines(a.j, sand.piles[, 5], col = "slateblue2", lty = 2)
lines(rep(x[5], 2), c(0, m), col = "grey50", lty = 3)
title(xlab = "a", ylab = "Density")
stripchart(x, method = "stack", pch = 20, col = "red", add = TRUE, cex = 2, at = 0)



# d)
piled.sand <- rowSums(sand.piles)
lines(a.j, piled.sand, col = "slateblue2", lwd = 2)


