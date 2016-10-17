# Section 2 
# Group H 
#   Scott Li
#   Junyang Liu
#   Ryan Quigley
# Problem 1

# Import with read.table.
psgrps <- read.table("ps05.txt", header = TRUE, sep = ",", stringsAsFactors = FALSE)

#a)_____________________________________________________________________________
# Compare split string with original string to determine if there is a last name
no.last <- seq_along(psgrps[[1]])[psgrps[[1]] == sapply(psgrps[1], strsplit, " ")]

#b)_____________________________________________________________________________
names <- sapply(psgrps[1], strsplit, " ")
# Take every name and split them into first and last
first <- c(); last <- c()
for (i in seq_along(names)) {
    first[i] <- unlist(strsplit(names[[i]], " "))[1]
    last[i] <- unlist(strsplit(names[[i]], " "))[2]
}
fullname <- cbind(first, last)

#c)_____________________________________________________________________________
# Create data frame
psgrps.x <- data.frame(first, last, psgrps[c(2,3,4)], stringsAsFactors = FALSE)
# Sort by first name
psgrps.x <- psgrps.x[order(psgrps.x[1]), ]
# Sort by last name. NA first. 
psgrps.x <- psgrps.x[order(psgrps.x[2], na.last = FALSE), ]


#d)_____________________________________________________________________________
# Rename the rows of psgrps.x for indexing 
rownames(psgrps.x) <- seq_along(psgrps.x[[1]])
# implementation 1:
grp.mates <- list()
length(grp.mates) <- 24
for (i in 1:24) {
	grp.ps1 <- psgrps.x$ps1[i]
	idx.ps1 <- psgrps.x$ps1 == grp.ps1
	idx.ps1[i] <- FALSE
	grp.mates.ps1 <- rownames(psgrps.x)[idx.ps1]
	grp.mates[[i]] <- as.integer(grp.mates.ps1)
	
	grp.ps2 <- psgrps.x$ps2[i]
	idx.ps2 <- psgrps.x$ps2 == grp.ps2
	idx.ps2[i] <- FALSE
	grp.mates.ps2 <- rownames(psgrps.x)[idx.ps2]
	grp.mates[[i]] <- c(grp.mates[[i]], as.integer(grp.mates.ps2))
}
names(grp.mates) <- psgrps.x$first




#e)_____________________________________________________________________________
# Number of group mates
num.grp.mates <- sapply(grp.mates, length)
# Determine unique group mates
unq.grp.mates <- lapply(grp.mates, unique)
# Number of unique group mates
num.unq.mates <- sapply(unq.grp.mates, length)
row.idx <- 1:24
repeats <- row.idx[num.unq.mates != 4]


#f)_____________________________________________________________________________
# Create vector of group letters for ps2
ps2 <- rep(LETTERS[1:8], times = 3)
# Order data frame by ps1
ord.ex <- order(psgrps.x$ps1, decreasing = FALSE)
# Remove original ps2 grouping
names.df.ex <- cbind(psgrps.x[ord.ex, -5], ps2)

# Re-sort according to instructions from part (c)
ord.ex <- order(names.df.ex[, 2], names.df.ex[, 1], na.last = FALSE, decreasing = FALSE)
psgrps.new <- names.df.ex[ord.ex, ]

