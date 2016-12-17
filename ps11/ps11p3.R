## PS11 
## Problem 3

## a)
load("~/Documents/SJSU/267A/final/ps11.Rdata")
str(x)
summary(x)
boxplot(x, horizontal = TRUE)
d <- density(x, bw = 2, from = -30, to = 90)
plot(0,0, type = "n", ann = FALSE, axes = FALSE, ylim = c(0,0.20), xlim = c(-30, 90))
hist(x, right = FALSE, freq = FALSE, breaks = 50, axes = FALSE, ann = FALSE, add = TRUE, border = "grey75", col = "grey99")
lines(d, col = "violetred", lwd = 1.5)
axis(1, seq.int(-30, 90, 10), col = "grey50")
axis(2, seq.int(0, 0.20, 0.05), col = "grey50")

qqnorm(x, pch = 21, col = "black", bg = "grey75", cex = 0.5)
qqline(x, lty = 3)
abline(a = 0, b = 1, col = "violetred", lty = 2)

## b) 
# estimation of SE
set.seed(2016)
n <- 100
B <- 10000
bs <- matrix(sample(x, size = n*B, replace = TRUE), nrow = B)
bs.means <- rowMeans(bs)
t.star <- mean(bs.means)
obs.stat <- mean(x)
bias.est <- t.star - mean(x)

cat("Estimate =",obs.stat, "\nEstimated bias =", bias.est, "\nBias-corrected estimate", obs.stat - bias.est)

db <- density(bs.means, bw = 1, from = -4, to = 8)
plot(0,0, type = "n", ann = FALSE, axes = FALSE, ylim = c(0,0.40), xlim = c(-4, 8))
lines(db, col = "violetred", lwd = 1.5)
axis(1, seq.int(-4, 8, 2), col = "grey50")
axis(2, seq.int(0, 0.40, 0.1), col = "grey50")
lines(c(t.star, t.star), c(0, max(db$y)), col = "grey75", lty = 3)

qqnorm(bs.means, pch = 21, col = "black", bg = "grey75", cex = 0.5)
qqline(bs.means, lty = 3, col = "violetred")
# Skew right compared to standard normal

### Appropriate graphical summmaries????????