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
# Should it be a list with 24 components...one component for each student?
grp.names <- list()
length(grp.names) <- 24
for (i in 1:24) {
	grp.letter <- psgrps.x$ps1[i]
	grp.mates <- rownames(psgrps.x)[psgrps.x$ps1 == grp.letter]
	grp.names[[i]] <- as.integer(grp.mates)
}
# OR should it have 8 components...one for each group?
grp.names <- split(row.names(psgrps.x), psgrps.x$ps1)
