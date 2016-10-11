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


psgrps.x.t <- psgrps.x[, -5]
rownames(psgrps.x.t) <- 1:24

grp.names <- list()
length(grp.names) <- 24
for (i in 1:24) {
	grp.ps1 <- psgrps.x.t$ps1[i]
	idx.ps1 <- psgrps.x.t$ps1 == grp.ps1
	idx.ps1[i] <- FALSE
	grp.mates.ps1 <- rownames(psgrps.x.t)[idx.ps1]
	grp.names[[i]] <- as.integer(grp.mates.ps1)
}
