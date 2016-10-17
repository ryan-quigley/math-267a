# Section 2
# Group X
#  Qiaoqiao Jiang
#  Yunyun Tao
#  Ryan Quigley
# Problem 2
#### PDF dimensions / aspect ratio????
pdf("ps06p2.pdf", width = 8, height = 5)
# Dataset
data(Cars93, package = "MASS")

# Plot 1: set up
h <- hist(Cars93$Price, plot = FALSE, right = FALSE)
bin.width <- unique(diff(h$breaks, lag = 1))
d <- c(0, h$count, 0) / (bin.width * sum(h$count))
diff.d <- diff(d, lag = 1)

black.dots <- cbind(c(0, h$breaks[diff.d != 0]), d[diff.d != 0])
white.dots <- cbind(black.dots[-1, 1], black.dots[-dim(black.dots)[1], 2])

# Plot 2: set up
new.breaks <- h$breaks - 2.5
h2 <- hist(Cars93$Price, breaks = new.breaks, plot = FALSE, right = FALSE)
bin.width2 <- unique(diff(h2$breaks, lag = 1))
d2 <- c(0, h2$count, 0) / (bin.width2 * sum(h2$count))
diff.d2 <- diff(d2, lag = 1)

black.dots2 <- cbind(c(0,h2$breaks[diff.d2 != 0]), d2[diff.d2 != 0])
white.dots2 <- cbind(black.dots2[-1, 1], black.dots2[-dim(black.dots2)[1], 2])

# Plot panel set up
par(mfcol = c(1,2))

# Plot 1: graphics
plot(black.dots, ann = FALSE, axes = FALSE, pch = 19, xlim = c(0,70), type = "n")
axis(1, at = seq.int(0, 70, by = 10), labels = seq.int(0, 70, by = 10))
axis(2, at = seq.int(0, 0.06, by = 0.02), labels = seq.int(0, 0.06, by = 0.02))

for (i in seq_along(black.dots[, 1])) {
	a <- black.dots[i, ]
	if (i == dim(black.dots)[1]) {
		b <- c(70, 0)
		c <- c(NA, NA)
		d <- c(NA, NA)
	} else {
		b <- white.dots[i, ]
		c <- white.dots[i, ]
		d <- black.dots[i + 1, ]
	}
	
	t <- rbind(a, b)
	lines(t, col = "violetred")
	u <- rbind(c, d)
	lines(u, col = "grey50", lty = 3)
}

points(black.dots, pch = 20, xlim = c(0,70))
points(white.dots, pch = 21, col = "black", bg = "white", cex = (2/3))
title(xlab = "a", ylab = "g(a)")



# Plot 2: graphics
plot(black.dots2, ann = FALSE, axes = FALSE, pch = 19, xlim = c(0, 70), ylim = c(0, 0.06), type = "n")
axis(1, at = seq.int(0, 70, by = 10), labels = seq.int(0, 70, by = 10))
axis(2, at = seq.int(0, 0.06, by = 0.02), labels = seq.int(0, 0.06, by = 0.02))

for (i in seq_along(black.dots2[, 1])) {
	e <- black.dots2[i, ]
	if (i == dim(black.dots2)[1]) {
		f <- c(70, 0)
		g <- c(NA, NA)
		h <- c(NA, NA)
	} else {
		f <- white.dots2[i, ]
		g <- white.dots2[i, ]
		h <- black.dots2[i + 1, ]
	}
	
	v <- rbind(e, f)
	lines(v, col = "violetred")
	w <- rbind(g, h)
	lines(w, col = "grey50", lty = 3)
}

points(black.dots2, pch = 20, xlim = c(0,70))
points(white.dots2, pch = 21, col = "black", bg = "white", cex = (2/3))
title(xlab = "a", ylab = "g(a)")


dev.off()