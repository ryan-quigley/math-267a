# Group B problem 2
# Ian Dupzyk
# Ryan Quigley
# Sucharu Gupta

# a)
tri.num.20 <- as.numeric(c(1:20))

for (i in tri.num.20[2:20]) {
    tri.num.20[i] <- tri.num.20[i-1]+i
}


# b)
x <- c(1:20)
tri.num.20 <- x*(x+1)/2
cat(tri.num.20, fill=TRUE)


# c)
N <- 100000

system.time({
    tri.num <- as.numeric(c(1:N))

    for (i in tri.num[2:length(tri.num)]) {
        tri.num[i] <- tri.num[i-1]+i
    }
})

system.time({
    x <- c(1:N)
    tri.num <- x*(x+1)/2
})

# d)
x <- c(1:50)
tri.num.50 <- x*(x+1)/2
nm <- unlist(list(letters, paste(letters, letters, sep='')))
length(nm) <- 50
names(tri.num.50) <- nm
tri.num.50

# e)
vowel_named_numbers <- tri.num.50[grep('[aeiou]', nm)]
