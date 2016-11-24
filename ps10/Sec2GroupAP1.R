# MATH 267A
# Section 2
# Group A
#  Navid Morshed
#  Han Xu
#  Ryan Quigley
# Problem 1

load("ps10p1.RData")

# a) it is given in the problem that the variance is known, sigma = sqrt(5)
# samples - rows
# values in each sample - cols

# Set-up
alpha <- 0.05
m <- 100
n <- 30
sigma2 <- 5

# CI
width <- qnorm(p = (1 - alpha/2))*sqrt(sigma2/n)
x.bar <- rowMeans(normal)
ci.bounds <- cbind(x.bar - width, x.bar + width)

par(mfrow = c(1,2))

# Plot
plot(1, 10, axes = FALSE, ann = FALSE, 
	xlim = c(floor(min(ci.bounds[,1])), ceiling(max(ci.bounds[,2]))),
	ylim = c(-3,110), 
	type = "n")
ci.fail <- 0
for (i in 1:m) {
	if (10 < ci.bounds[i, 1] | 10 > ci.bounds[i, 2]) {
		clr <- "violetred"
		ci.fail <- ci.fail + 1
	} else {
		clr <- "gray75"
	}
	lines(ci.bounds[i, ], rep(i+7,2), col = clr)
}
lines(rep(10, 2), c(0, 110), col = "gray50", lwd = 1.5, lty = 3)
axis(1, labels = FALSE, tick = TRUE, lwd = 1.5, lwd.tick = 0, col = "gray50", pos = 0)
axis(1, at = 10, labels = 10, lwd = 0, lwd.tick = 1.5, col = "gray50", pos = 0)

cat(ci.fail,"/100 fail to capture the true mean", sep = "")
cat("Length of interval: ", 2*width)



# b)
alpha <- 0.20
m <- 100
n <- 30
sigma2 <- 5

# CI
width <- qnorm(p = (1 - alpha/2))*sqrt(sigma2/n)
x.bar <- rowMeans(normal)
ci.bounds <- cbind(x.bar - width, x.bar + width)

# Plot
plot(1, 10, axes = FALSE, ann = FALSE, 
	xlim = c(floor(min(ci.bounds[,1])), ceiling(max(ci.bounds[,2]))),
	ylim = c(-3,110), 
	type = "n")
ci.fail <- 0
for (i in 1:m) {
	if (10 < ci.bounds[i, 1] | 10 > ci.bounds[i, 2]) {
		clr <- "violetred"
		ci.fail <- ci.fail + 1
	} else {
		clr <- "gray75"
	}
	lines(ci.bounds[i, ], rep(i+7,2), col = clr)
}
lines(rep(10, 2), c(0, 110), col = "gray50", lwd = 1.5, lty = 3)
axis(1, labels = FALSE, tick = TRUE, lwd = 1.5, lwd.tick = 0, col = "gray50", pos = 0)
axis(1, at = 10, labels = 10, lwd = 0, lwd.tick = 1.5, col = "gray50", pos = 0)

cat(ci.fail,"/100 fail to capture the true mean", sep = "")
cat("Length of interval: ", 2*width)

# c)
p <- 0.05
p*n # condition for Wald interval is NOT met, thus the normal approximation is not appropriate
(1-p)*n

library(binom)

num.success <- rowSums(bernoulli)
ci.wald <- binom.confint(num.success, n, conf.level = 0.95, methods = "asymptotic")
ci.wilson <- binom.confint(num.success, n, conf.level = 0.95, methods = "wilson")

par(mfrow = c(1,2))
# Plot: wald
plot(0.05, 10, axes = FALSE, ann = FALSE, 
	xlim = c(-0.05, 0.3),
	ylim = c(-3,110), 
	type = "n")
lines(rep(0.05, 2), c(0, 110), col = "gray50", lwd = 1.5, lty = 3)
lines(rep(0, 2), c(0, 110), col = "gray50", lwd = 1.5, lty = 3)
ci.fail <- 0
ci.zero <- 0
for (i in 1:m) {
	if (ci.wald$x[i] == 0){
		points(0, i+7, col = "violetred", pch = 20, cex = 0.50)
		ci.zero <- ci.zero + 1
	} else if (0.05 < ci.wald[i, 5] | 0.05 > ci.wald[i, 6]) {
		clr <- "violetred"
		ci.fail <- ci.fail + 1
		lines(ci.wald[i, c(5, 6)], rep(i+7,2), col = clr)
	} else {
		clr <- "gray75"
		lines(ci.wald[i, c(5, 6)], rep(i+7,2), col = clr)
	}
}
axis(1, labels = FALSE, tick = TRUE, lwd = 1.5, lwd.tick = 0, col = "gray50", pos = 0)
axis(1, at = c(0, 0.05), labels = c(0, 0.05), lwd = 0, lwd.tick = 1.5, col = "gray50", pos = 0)

cat(ci.fail," non-zero intervals fail to capture the true proportion", sep = "")
cat(ci.zero," intervals could not be calculated", sep = "")

# Plot: wilson
plot(0.05, 10, axes = FALSE, ann = FALSE, 
	xlim = c(-0.05, 0.3),
	ylim = c(-3,110), 
	type = "n")
lines(rep(0.05, 2), c(0, 110), col = "gray50", lwd = 1.5, lty = 3)
lines(rep(0, 2), c(0, 110), col = "gray50", lwd = 1.5, lty = 3)
ci.fail <- 0
for (i in 1:m) {
	if (0.05 < ci.wilson[i, 5] | 0.05 > ci.wilson[i, 6]) {
		clr <- "violetred"
		ci.fail <- ci.fail + 1
	} else {
		clr <- "gray75"
	}
	lines(ci.wilson[i, c(5, 6)], rep(i+7,2), col = clr)
}
axis(1, labels = FALSE, tick = TRUE, lwd = 1.5, lwd.tick = 0, col = "gray50", pos = 0)
axis(1, at = c(0, 0.05), labels = c(0, 0.05), lwd = 0, lwd.tick = 1.5, col = "gray50", pos = 0)

cat(ci.fail,"/100 Wilson intervals fail to capture the true proportion", sep = "")

