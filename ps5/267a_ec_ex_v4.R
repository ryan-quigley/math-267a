# all combinations
x <- t(combn(24, 3))

# check all combinations
idx.mat <- integer(0)

i <- 1
while (length(idx.mat) < 24) {
	grp <- x[i, ]
	if (!any(grp %in% idx.mat)) {
		idx.mat <- c(idx.mat, grp)
		idx.vec <- integer(0)
		for (k in 1:(dim(x)[1])) {
			test <- x[k, ]
			if (sum(test %in% grp) >= 2) {
				idx.vec <- c(idx.vec, k)
			}
		}
		x <- x[-idx.vec, ]
		i <- 1
	} else i <- i + 1
	idx.mat
}





idx.complete <- c(idx.complete, idx.mat)
