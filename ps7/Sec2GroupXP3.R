# Section 2
# Group X
#  Y
#  Z
#  Ryan Quigley
# Problem 3

# Calcs for P2 (Remove before submitting)
((2.25 - 1)/0.8)
(3/4)*(1 - ((2.25 - 1)/0.8)^2)

((2.25 - 1.2)/0.8)
(3/4)*(1 - ((2.25 - 1.2)/0.8)^2)

((2.25 - 1.5)/0.8)
(3/4)*(1 - ((2.25 - 1.5)/0.8)^2)

((2.25 - 2.8)/0.8)
(3/4)*(1 - ((2.25 - 2.8)/0.8)^2)

((2.25 - 3)/0.8)
(3/4)*(1 - ((2.25 - 3)/0.8)^2)

(1/(5*0.8))*((3/4)*(1 - ((2.25 - 1.5)/0.8)^2) + (3/4)*(1 - ((2.25 - 2.8)/0.8)^2) + (3/4)*(1 - ((2.25 - 3)/0.8)^2))


# Code for Problem 3
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
	Kh <- numeric(length(a))
	u <- (a - x)/h
	u.in <- u[abs(u) < 1]
	Kh.in <- (1/h)*(3/4)*(1 - u.in^2)
	Kh[abs(u) < 1] <- Kh.in
	Kh
}


epanechnikov(1.2)
epanechnikov(c(1.2, 1.5, 1.8, 0.8))
epanechnikov(1.2, x = 1, h = 0.5)
epanechnikov(c(1.2, 1.5, 1.8, 0.8), x = 1, h = 0.5)
epanechnikov(c(1.2, 1.5, 1.8, 0.8), x = 0.5, h = 0.5)
epanechnikov(c(1.2, 1.5, 1.8, 0.8), x = 1.5, h = 0.5)
epanechnikov(1.2, x = 2.8, h = 0.5)
epanechnikov("1.2")
epanechnikov(1.2, x = "1", h = 0.5)
epanechnikov(1.2, x = 2.8, h = -0.5)

# b)
curve(epanechnikov(a, x = 0, h = 1), from = -3, to = 3, xname = "a", xlim = c(-3,3), ylim = c(0,2), axes = FALSE, ann = FALSE, col = "violetred")
curve(epanechnikov(a, x = 0, h = 0.5), from = -3, to = 3, xname = "a", add = TRUE, col = "slateblue2")
curve(epanechnikov(a, x = 0, h = 2), from = -3, to = 3, xname = "a", add = TRUE, col = "gold")
curve(epanechnikov(a, x = 1, h = 0.75), from = -3, to = 3, xname = "a", add = TRUE, col = "springgreen")
axis(1, at = seq.int(-3, 3), labels = seq.int(-3, 3))
axis(2, at = seq.int(0, 2, by = 0.5), seq.int(0, 2, by = 0.5))
expr1 <- expression(K[1](a))
expr2 <- expression(K[0.5](a))
expr3 <- expression(K[2](a))
expr4 <- expression(K[0.75](a - 1))
legend(1, 2, legend = c(expr1, expr2, expr3, expr4), bty = "n", lty = 1, col = c("violetred", "slateblue2", "gold", "springgreen"))
title(xlab = "a", ylab = "Density")

# c) 
# What does she mean by "do not perform the computation for each observation"? 
# Does the (a) function need to be modified to accept a vector for x?
x <- c(1, 1.2, 1.5, 2.8, 3)
a.j <- seq.int(0, 4, length.out = 500)
h = 0.75
xK1 <- (1/5)*epanechnikov(a.j, x = x[1], h = h)
xK2 <- (1/5)*epanechnikov(a.j, x = x[2], h = h)
xK3 <- (1/5)*epanechnikov(a.j, x = x[3], h = h)
xK4 <- (1/5)*epanechnikov(a.j, x = x[4], h = h)
xK5 <- (1/5)*epanechnikov(a.j, x = x[5], h = h)
sand.piles <- cbind(xK1, xK2, xK3, xK4, xK5)
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


