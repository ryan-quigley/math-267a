# MATH 267A
# Section 2
# Group A
#  Navid Morshed
#  Han Xu
#  Ryan Quigley
# Problem 1

load("ps10p1.RData")

# a) 
# It is given in the problem that the variance is known: sigma = sqrt(5)
# samples - rows
# values in each sample - cols

# Set-up
alpha <- 0.05
m <- 100
n <- 30
sigma2 <- 25

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
title(main = "95% Confidence Intervals (100 samples)", sub = paste(ci.fail,"/100 fail to capture true mean", sep = ""), line = 2)



# b)
alpha <- 0.20
m <- 100
n <- 30
sigma2 <- 25

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
title(main = "80% Confidence Intervals (100 samples)", sub = paste(ci.fail,"/100 fail to capture true mean", sep = ""), line = 2)


# c)

p <- 0.05
# Condition for Wald interval is NOT met, thus the normal approximation is not appropriate
wald.c.1 <- p*n 
wald.c.2 <- (1-p)*n

num.success <- rowSums(bernoulli)
ci.wald <- binom::binom.confint(num.success, n, conf.level = 0.95, methods = "asymptotic")
ci.wilson <- binom::binom.confint(num.success, n, conf.level = 0.95, methods = "wilson")

margins <- par("mar")
par(mfrow = c(1,2), mar = margins + c(0,-3,0,-1))
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
title(main = "95% Wald Intervals (100 samples)", sub = paste(ci.fail + ci.zero,"% fail to capture true proportion", sep = ""), line = 2)


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
title(main = "95% Score Intervals (100 samples)", sub = paste(ci.fail,"% fail to capture true proportion", sep = ""), line = 2)
