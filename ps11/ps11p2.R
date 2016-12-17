## PS11
## Problem 2

dataset <- 1:3
indx <- expand.grid(rep(list(1:3), 3))
bs <- expand.grid(rep(list(dataset), 3))
bs.mean <- apply(bs, 1, mean)
bs.table <- cbind.data.frame(indx, bs,
    round(bs.mean, digits = 2))

# Counting occurences
l <- split(bs.table[, 4:6], seq(nrow(bs.table)))
names(l) <- NULL
l <- lapply(l, as.integer)
l <- lapply(l, sort)

f2 <- function(x) {
	lapply(l, identical, x)
}

lol <- lapply(l, f2)

f3 <- function(x) {
	z <- unlist(x)
	sum(z)
}
counts <- unlist(lapply(lol, f3))
bs.table$counts <- counts
bs.table$probs <- round(bs.table$counts / nrow(bs.table), 2)

# Column name clean up
colnames(bs.table) <- c(paste0("i", 1:3),
    paste0("x", 1:3), "mean", "counts", "probs")
print(bs.table[1:5, ], row.names = FALSE)

# Boostrap distribution of x.bar (mean)
bsd.mean <- table(bs.mean, dnn = NULL)
# Name cleanup
row.names(bsd.mean) <- round(sort(unique(bs.mean)), 
	digits = 2)
# pmf
bsd.mean.pmf <- round(bsd.mean/sum(bsd.mean), 2)
a1 <- as.numeric(names(bsd.mean.pmf))
E.bsd.mean <- sum(a1*bsd.mean.pmf)
weighted.mean(a1, bsd.mean.pmf)
se.bsd.mean <- sqrt(sum((a1^2)*bsd.mean.pmf) - E.bsd.mean^2)
#sd(bs.table$mean) # not the same

# Boostrap distribution of x.tilde (median)
bs.median <- apply(bs, 1, median)
bs.table$median <- bs.median
bsd.medn <- table(bs.median, dnn = NULL)
bsd.medn.pmf <- round(bsd.medn/sum(bsd.medn), 2)
a2 <- as.numeric(names(bsd.medn.pmf))
E.bsd.medn <- sum(a2*bsd.medn.pmf)
se.bsd.medn <- sqrt(sum((a2^2)*bsd.medn.pmf) - E.bsd.medn^2)
