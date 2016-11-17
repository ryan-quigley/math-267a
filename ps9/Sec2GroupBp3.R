# MATH 267A
# Section 2
# Group D
#  Beatriz Hernandez
#  Jimmy Nguyen
#  Ryan Quigley
# Problem 3


load("ps9p3.RData")
summary(x)
m <- mean(x)
s <- sd(x)


#hist(x, breaks = 20, freq = FALSE, right = FALSE, col = "royalblue", border = "aquamarine", xlim = c(0,1))


h <- 0.04
krn <- "triangular"
d <- density(x, kernel = krn, bw = h, from = -0.5, to = 1.5)

y.up <- ceiling(max(d$y)) + 1
plot(d, axes = FALSE, ann = FALSE, ylim = c(0, y.up), col = "violetred")
axis(1, at = seq.int(-0.5, 1.5, 0.5), col = "grey50")
axis(2, at = seq.int(0, y.up, 0.5), col = "grey50")

curve(dnorm(x, mean = m, sd = s), from = -0.5, to = 1.5, col = "slateblue2", lty = 2, add = TRUE)
curve(dbeta(x, shape1 = 7.5, shape2 = 6.5), from = 0, to = 1, col = "black", lty = 3, add = TRUE)

legend("topright", inset = 0.05, 
	legend = c(paste(krn, "(h = ", h, ")", sep = ""), 
		paste("N(", round(m,3),", ",round(s^2,3),")", sep = ""),
		paste("Beta(7.5, 6.5)")
	), 
	bty = "n", col = c("violetred", "slateblue2", "black"), 
	lty = c(1,2,3), 
	lwd = 1)
title(xlab = "x", ylab = "Density")


# x2 <- rnorm(length(x)*1000, mean = m, sd = s)
# d2 <- density(x2, kernel = krn, bw = h, from = -0.5, to = 1.5)
# lines(d2)
