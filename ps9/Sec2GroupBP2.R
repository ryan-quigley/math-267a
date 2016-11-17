# MATH 267A
# Section 2
# Group D
#  Beatriz Hernandez
#  Jimmy Nguyen
#  Ryan Quigley
# Problem 2


x <- c(-1,0,0,1,1,2,2,3,3,4)
y <- c(0,0,0,1/4,1/2,1/2,2/3,3/4,1,1)
open <- c(0,0,0,1,0,1,0,1,0,0)
closed <- c(0,0,0,0,1,0,1,0,1,0)
df <- data.frame(x, y, open, closed)
n <- dim(df)[1] - 1

plot(df$x, df$y, type = "n", axes = FALSE, ann = FALSE)
for (i in 1:n) {
	if (df$x[i] != df$x[i+1]) {
		lines(df[c(i, i+1), 1:2], col = "violetred")
	} else {
		lines(df[c(i, i+1), 1:2], lty = 3, col = "grey50")
	}
}
points(df[df$closed == 1, 1:2], pch = 20)
points(df[df$open == 1, 1:2], pch = 21, col = "black", bg = "white", cex = (2/3))

axis(1, at = seq.int(-1,4), col = "grey50", lwd = 0, lwd.tick = 1)
box(lty = 1, col = "grey50")
axis(2, at = seq.int(0, 1, 0.2), lwd = 0, lwd.tick = 1, col = "grey50")
title(main = "CDF of Random Variable X", xlab = "x", ylab = "F(x)")
