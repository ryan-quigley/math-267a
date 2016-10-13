# Section 2 
# Group H 
#  A
#  Scott Li
#  Ryan Quigley
# Problem 1

psgrps <- read.csv("ps05.txt", stringsAsFactors = FALSE)

# a)
name.split <- strsplit(psgrps$name, " ")
name.nums <- sapply(name.split, length)
no.last <- as.integer(rownames(psgrps)[name.nums == 1])


# b)
f <- function(x) {
	y <- x
	length(y) <- 2
	y
}
names.list <- lapply(name.split, f)
fullname <- matrix(unlist(names.list), ncol = 2, byrow = TRUE)


# c)
dimnames(fullname) <- list(NULL, c("first", "last"))
names.df <- cbind(fullname, psgrps[2:4])
ord <- order(names.df[, 2], names.df[, 1], na.last = FALSE, decreasing = FALSE)
psgrps.x <- names.df[ord, ]



# d)
# rename rows
rownames(psgrps.x) <- 1:24
# implementation 1:
grp.names <- list()
length(grp.names) <- 24
for (i in 1:24) {
	grp.ps1 <- psgrps.x$ps1[i]
	idx.ps1 <- psgrps.x$ps1 == grp.ps1
	idx.ps1[i] <- FALSE
	grp.mates.ps1 <- rownames(psgrps.x)[idx.ps1]
	grp.names[[i]] <- as.integer(grp.mates.ps1)
	
	grp.ps2 <- psgrps.x$ps2[i]
	idx.ps2 <- psgrps.x$ps2 == grp.ps2
	idx.ps2[i] <- FALSE
	grp.mates.ps2 <- rownames(psgrps.x)[idx.ps2]
	grp.names[[i]] <- c(grp.names[[i]], as.integer(grp.mates.ps2))
}
names(grp.names) <- psgrps.x$first




# e)
l <- sapply(grp.names, length)
num.mates <- unique(l)
u <- lapply(grp.names, unique)
lu <- sapply(u, length)
row.idx <- 1:24
repeats <- row.idx[lu != 4]


# f)
ps2 <- rep(LETTERS[1:8], times = 3)
ord.grps <- order(psgrps.x$ps1, decreasing = FALSE)
names.df.ord <- cbind(psgrps.x[ord.grps, -5], ps2)

# resort according to instructions from part (c)
ord.c <- order(names.df.ord[, 2], names.df.ord[, 1], na.last = FALSE, decreasing = FALSE)
psgrps.new <- names.df.ord[ord.c, ]

# check repeats
grp.names <- list()
length(grp.names) <- 24
for (i in 1:24) {
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
}


l <- sapply(grp.names, length)
num.mates <- unique(l)
u <- lapply(grp.names, unique)
lu <- sapply(u, length)
row.idx <- 1:24
repeats <- row.idx[lu != 4]

