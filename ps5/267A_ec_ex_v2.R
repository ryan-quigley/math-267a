#### Set Up ###

psgrps.t <- read.table("~/Documents/SJSU/267A/homework_scripts/math-267a/ps5/ps05_ec.txt", header = TRUE)


### Algo ###

idx <- 1:24
idx.code <- c(1, 5, 9, 4, 8, 12, 7, 11, 15, 10, 14, 18, 13, 17, 21, 16, 20, 24, 19, 23, 3, 22, 2, 6)
idx.new <- idx
idx.mat <- integer(0)
repeat {
	idx.new <- idx.new[idx.code]
	idx.mat <- c(idx.mat, idx.new)
	if (length(idx.mat)/24 == 3) {
		break
	}
}

## Diagnostic
matrix(idx.mat, ncol = 24, byrow = TRUE)
matrix(1:24, ncol = 3, byrow = TRUE)
# dim(b) <- NULL
 

##
psgrps.complete <- psgrps.t
for (i in 1:3) {
	g <- idx.mat[1:24]
	idx.mat <- idx.mat[-(1:24)]
	ps.mat <- matrix(g, ncol = 3, byrow = TRUE)
	ps.grp <- split(ps.mat, LETTERS[1:8])
	ps <- character(24)
	for (j in 1:8) {
		ps[ ps.grp[[j]] ] <- names(ps.grp)[j]
	}
	psgrps.complete <- data.frame(psgrps.complete, ps = ps)
}

dim(psgrps.complete)
names(psgrps.complete) <- c(names(psgrps.complete)[1:4], paste("ps", 2:4, sep = ""))

grp.names <- list()
length(grp.names) <- 24
for (i in seq_along(grp.names)) {
	grp.ps1 <- psgrps.complete$ps1[i]
	idx.ps1 <- psgrps.complete$ps1 == grp.ps1
	idx.ps1[i] <- FALSE
	grp.mates.ps1 <- rownames(psgrps.complete)[idx.ps1]
	grp.names[[i]] <- as.integer(grp.mates.ps1)
	
	grp.ps2 <- psgrps.complete$ps2[i]
	idx.ps2 <- psgrps.complete$ps2 == grp.ps2
	idx.ps2[i] <- FALSE
	grp.mates.ps2 <- rownames(psgrps.complete)[idx.ps2]
	grp.names[[i]] <- c(grp.names[[i]], as.integer(grp.mates.ps2))
	
	grp.ps3 <- psgrps.complete$ps3[i]
	idx.ps3 <- psgrps.complete$ps3 == grp.ps3
	idx.ps3[i] <- FALSE
	grp.mates.ps3 <- rownames(psgrps.complete)[idx.ps3]
	grp.names[[i]] <- c(grp.names[[i]], as.integer(grp.mates.ps3))
	
	grp.ps4 <- psgrps.complete$ps4[i]
	idx.ps4 <- psgrps.complete$ps4 == grp.ps4
	idx.ps4[i] <- FALSE
	grp.mates.ps4 <- rownames(psgrps.complete)[idx.ps4]
	grp.names[[i]] <- c(grp.names[[i]], as.integer(grp.mates.ps4))
	
	# grp.ps5 <- psgrps.complete$ps5[i]
	# idx.ps5 <- psgrps.complete$ps5 == grp.ps5
	# idx.ps5[i] <- FALSE
	# grp.mates.ps5 <- rownames(psgrps.complete)[idx.ps5]
	# grp.names[[i]] <- c(grp.names[[i]], as.integer(grp.mates.ps5))
	
	# grp.ps6 <- psgrps.complete$ps6[i]
	# idx.ps6 <- psgrps.complete$ps6 == grp.ps6
	# idx.ps6[i] <- FALSE
	# grp.mates.ps6 <- rownames(psgrps.complete)[idx.ps6]
	# grp.names[[i]] <- c(grp.names[[i]], as.integer(grp.mates.ps6))
	
	# grp.ps7 <- psgrps.complete$ps7[i]
	# idx.ps7 <- psgrps.complete$ps7 == grp.ps7
	# idx.ps7[i] <- FALSE
	# grp.mates.ps7 <- rownames(psgrps.complete)[idx.ps7]
	# grp.names[[i]] <- c(grp.names[[i]], as.integer(grp.mates.ps7))
}

l <- sapply(grp.names, length)
num.mates <- unique(l)
u <- lapply(grp.names, unique)
lu <- sapply(u, length)
row.idx <- 1:24
repeats <- row.idx[lu != num.mates]
if (length(repeats) == 0) {
	cat("SUCCESS!")
}



# Determing possible group members for each person
f <- function(x, ps) {
	ps[-x]
}
possibles <- 1:24
x <- lapply(grp.names, f, ps = possibles)
for (i in seq_along(x)) {
	x[[i]] <- x[[i]][x[[i]] != i]
}