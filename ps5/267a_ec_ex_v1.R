#### Set Up ###

psgrps <- read.csv("ps05.txt", stringsAsFactors = FALSE)

name.split <- strsplit(psgrps$name, " ")
name.nums <- sapply(name.split, length)
no.last <- as.integer(rownames(psgrps)[name.nums == 1])

f <- function(x) {
	y <- x
	length(y) <- 2
	y
}
names.list <- lapply(name.split, f)
fullname <- matrix(unlist(names.list), ncol = 2, byrow = TRUE)



dimnames(fullname) <- list(NULL, c("first", "last"))
names.df <- cbind(fullname, psgrps[2:4])
ord <- order(names.df[, 2], names.df[, 1], na.last = FALSE, decreasing = FALSE)
psgrps.x <- names.df[ord, ]



########################################################
#### ALGO 1 (FAIL)####
# df set up
psgrps.trim <- psgrps.x[, -5]


# get ps1 grp names
grp.names <- list()
length(grp.names) <- 24
for (i in seq_along(grp.names)) {
	grp.ps1 <- psgrps.trim$ps1[i]
	idx.ps1 <- psgrps.trim$ps1 == grp.ps1
	idx.ps1[i] <- FALSE
	grp.mates.ps1 <- rownames(psgrps.trim)[idx.ps1]
	grp.names[[i]] <- as.integer(grp.mates.ps1)
}

# algo for ps2 grps
possibles <- 1:24
j <- 1
grps <- LETTERS[1:8]
grps.vec <- rep(NA, 24)
for (i in 1:24) {
	if (i %in% possibles) {
		grp <- grps[j]
		gm1 <- i
		possibles <- possibles[possibles != gm1]
		gm2 <- min(possibles[!(possibles %in% grp.names[[gm1]])])
		possibles <- possibles[possibles != gm2]
		x <- c(grp.names[[gm1]], grp.names[[gm2]])
		gm3 <- min(possibles[!(possibles %in% x)])
		possibles <- possibles[possibles != gm3]
		grps.vec[c(gm1, gm2, gm3)] <- grp
		j <- j + 1
	} 
}

# check repeats as far as ps2

ps2 <- grps.vec
psgrps.new <- cbind(psgrps.trim, ps2)


grp.names <- list()
length(grp.names) <- 24
for (i in seq_along(grp.names)) {
	grp.ps1 <- psgrps.new$ps1[i]
	idx.ps1 <- psgrps.new$ps1 == grp.ps1
	idx.ps1[i] <- FALSE
	grp.mates.ps1 <- rownames(psgrps.new)[idx.ps1]
	grp.names[[i]] <- as.integer(grp.mates.ps1)
	grp.ps2 <- psgrps.new $ps2[i]
	idx.ps2 <- psgrps.new $ps2 == grp.ps2
	idx.ps2[i] <- FALSE
	grp.mates.ps2 <- rownames(psgrps.new)[idx.ps2]
	grp.names[[i]] <- c(grp.names[[i]], as.integer(grp.mates.ps2))
}


l <- sapply(grp.names, length)
u <- lapply(grp.names, unique)
lu <- sapply(u, length)
rep.list <- u[lu < l]
repeats <- unique(unlist(rep.list))
if (is.null(repeats)) {
	cat("SUCCESS!")
}


# algo for ps3
f2 <- function(x, ps) {
	ps[-x]
}

possibles <- 1:24
k <- 1
j <- 1
grps <- LETTERS[1:8]
grps.vec <- rep(NA, 24)
grps.test <- grp.names
names(grps.test) <- 1:24
while (any(is.na(grps.vec))) {
	a <- lapply(grps.test, f2, ps = possibles)
	b <- sapply(a, length) - 1
	print(b)
	c <- match(min(b[b > 0]), b)
	
	if (c %in% possibles) {
		grp <- grps[j]
		gm1 <- c
		possibles <- possibles[possibles != gm1]
		gm2 <- min(possibles[!(possibles %in% grps.test[[gm1]])])
		possibles <- possibles[possibles != gm2]
		x <- c(grps.test[[gm1]], grps.test[[gm2]])
		gm3 <- min(possibles[!(possibles %in% x)])
		possibles <- possibles[possibles != gm3]
		grps.vec[c(gm1, gm2, gm3)] <- grp
		j <- j + 1
	} else {
		grp <- grps[j]
		gm1 <- min(possibles)
		possibles <- possibles[possibles != gm1]
		gm2 <- min(possibles[!(possibles %in% grps.test[[gm1]])])
		possibles <- possibles[possibles != gm2]
		x <- c(grps.test[[gm1]], grps.test[[gm2]])
		gm3 <- min(possibles[!(possibles %in% x)])
		possibles <- possibles[possibles != gm3]
		grps.vec[c(gm1, gm2, gm3)] <- grp
		j <- j + 1
	}
	k <- k + 1
	if (k > 24) {
		break
	}
}


# check repeats as far as ps3

ps3 <- grps.vec
psgrps.new <- cbind(psgrps.new, ps3)

grp.names <- list()
length(grp.names) <- 24
for (i in seq_along(grp.names)) {
	grp.ps1 <- psgrps.new$ps1[i]
	idx.ps1 <- psgrps.new$ps1 == grp.ps1
	idx.ps1[i] <- FALSE
	grp.mates.ps1 <- rownames(psgrps.new)[idx.ps1]
	grp.names[[i]] <- as.integer(grp.mates.ps1)
	grp.ps2 <- psgrps.new$ps2[i]
	idx.ps2 <- psgrps.new$ps2 == grp.ps2
	idx.ps2[i] <- FALSE
	grp.mates.ps2 <- rownames(psgrps.new)[idx.ps2]
	grp.names[[i]] <- c(grp.names[[i]], as.integer(grp.mates.ps2))
	grp.ps3 <- psgrps.new$ps3[i]
	idx.ps3 <- psgrps.new$ps3 == grp.ps3
	idx.ps3[i] <- FALSE
	grp.mates.ps3 <- rownames(psgrps.new)[idx.ps3]
	grp.names[[i]] <- c(grp.names[[i]], as.integer(grp.mates.ps3))
}


l <- sapply(grp.names, length)
u <- lapply(grp.names, unique)
lu <- sapply(u, length)
rep.list <- u[lu < l]
repeats <- unique(unlist(rep.list))
if (is.null(repeats)) {
	cat("SUCCESS!")
}


########################################################
#### ALGO 2 ####
# df set up
psgrps.trim <- psgrps.x[, -5]
psgrps.trim <- psgrps.trim[order(psgrps.trim[,4], decreasing = FALSE), ]
rownames(psgrps.trim) <- 1:24

# get ps1 grp names
grp.names <- list()
length(grp.names) <- 24
for (i in seq_along(grp.names)) {
	grp.ps1 <- psgrps.trim$ps1[i]
	idx.ps1 <- psgrps.trim$ps1 == grp.ps1
	idx.ps1[i] <- FALSE
	grp.mates.ps1 <- rownames(psgrps.trim)[idx.ps1]
	grp.names[[i]] <- as.integer(grp.mates.ps1)
}


f3 <- function(x) {
	temp <- rep(0, 24)
	temp[x] <- 1
	temp
}
m <- sapply(grp.names, f3)
# note the matrix is always symmetric
# need to add 2 matrices such that no entries in the resulting matrix are equal to 2

m.df <- as.data.frame(m)

# 1  2  3
# 4  5  6
# 7  8  9
# 10 11 12
# 13 14 15
# 16 17 18
# 19 20 21
# 22 23 24

# 1  5  9
# 4  8  12
# 7  11 15
# 10 14 18
# 13 17 21
# 16 20 24
# 19 23 3
# 22 2  6

# 1  8  15
# 4  11 18
# 7  14 21
# 10 17 24
# 13 20 3
# 16 23 6
# 19 2  9
# 22 5  12

ps1.m <- matrix(1:24, ncol = 3, byrow = TRUE)
idx.ps1 <- 1:24 
idx.code <- c(1, 5, 9, 4, 8, 12, 7, 11, 15, 10, 14, 18, 13, 17, 21, 16, 20, 24, 19, 23, 3, 22, 2, 6)
idx.ps2 <- idx.ps1[idx.code]
ps2.m <- matrix(idx.ps2 , ncol = 3, byrow = TRUE)
idx.ps3 <- idx.ps2[idx.code]
ps3.m <- matrix(idx.ps3 , ncol = 3, byrow = TRUE)

ps1.grp.idx <- split(ps1.m, LETTERS[1:8])
ps2.grp.idx <- split(ps2.m, LETTERS[1:8])
ps3.grp.idx <- split(ps3.m, LETTERS[1:8])
ps1 <- character(24)
ps2 <- character(24)
ps3 <- character(24)
for (i in 1:8) {
	ps1[ ps1.grp.idx[[i]] ] <- names(ps1.grp.idx)[i]
	ps2[ ps2.grp.idx[[i]] ] <- names(ps2.grp.idx)[i]
	ps3[ ps3.grp.idx[[i]] ] <- names(ps3.grp.idx)[i]
}

psgrps.complete <- cbind(psgrps.trim, ps2, ps3)

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
}
l <- sapply(grp.names, length)
u <- lapply(grp.names, unique)
lu <- sapply(u, length)
rep.list <- u[lu < l]
repeats <- unique(unlist(rep.list))
if (is.null(repeats)) {
	cat("SUCCESS!")
}

