# Section 2 
# Group X 
#  A
#  B
#  Ryan Quigley
# Problem 1

psgrps <- read.csv("ps05.txt", stringsAsFactors = FALSE)

# a)
name.split <- strsplit(psgrps$name, " ")
name.nums <- sapply(name.split, length)
no.last <- as.integer(rownames(psgrps)[name.nums == 1])


# b)
# For loop
fullname <- matrix(NA, nrow = 24, ncol = 2)
for (i in seq_along(name.split)) {
	fullname[i, 1] <- name.split[[i]][1]
	fullname[i, 2] <- name.split[[i]][2] 
}

# Better version?
two.names.vec <- unlist(name.split[-no.last])
one.names.vec <- unlist(name.split[no.last])
two.names.mat <- matrix(two.names.vec, ncol = 2, byrow = TRUE)
one.nas <- rep(NA, times = length(one.names.vec))
one.names.mat <- matrix(c(one.names.vec, one.nas), ncol = 2)
fullname <- rbind(two.names.mat, one.names.mat)

# Best version??
# can we write a function of our own for this part?
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

# CHECK
print(psgrps.x, row.names = FALSE) # GOOD


# d)
# Should the row.names of psgrps.x be reindexed?
# Should it be a list with 24 components...one component for each student? Probably yes...
grp.names <- list()
length(grp.names) <- 24
for (i in 1:24) {
	grp.ps1 <- psgrps.x$ps1[i]
	grp.mates.ps1 <- rownames(psgrps.x)[psgrps.x[-i,]$ps1 == grp.ps1]
	grp.names[[i]] <- as.integer(grp.mates.ps1)
	grp.ps2 <- psgrps.x$ps2[i]
	grp.mates.ps2 <- rownames(psgrps.x)[psgrps.x[-i,]$ps2 == grp.ps2]
	grp.names[[i]] <- c(grp.names[[i]], as.integer(grp.mates.ps2))
}
# But can it be done without a for loop? Probably yes

# OR should it have 8 components...one for each group? Probably not
grp.names <- split(row.names(psgrps.x), psgrps.x$ps1)


# e)
l <- sapply(grp.names, length)
u <- lapply(grp.names, unique)
lu <- sapply(u, length)
# grp.names[lu < l]
rep.list <- u[lu < l]
repeats <- unique(unlist(rep.list))


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
	grp.mates.ps1 <- rownames(psgrps.new)[psgrps.new[-i,]$ps1 == grp.ps1]
	grp.names[[i]] <- as.integer(grp.mates.ps1)
	grp.ps2 <- psgrps.new $ps2[i]
	grp.mates.ps2 <- rownames(psgrps.new)[psgrps.new[-i,]$ps2 == grp.ps2]
	grp.names[[i]] <- c(grp.names[[i]], as.integer(grp.mates.ps2))
}
l <- sapply(grp.names, length)
u <- lapply(grp.names, unique)
lu <- sapply(u, length)
# grp.names[lu < l]
rep.list <- u[lu < l]
( repeats <- unique(unlist(rep.list)) )
# Repeats gives NULL

