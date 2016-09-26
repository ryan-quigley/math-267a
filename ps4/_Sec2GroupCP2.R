# Section 2
# Group C
#  Jennifer Chu
#  Samnang Tep
#  Ryan Quigley
# Problem 2

scan("caffeine.txt")

caffeine <- read.table("caffeine.txt", header = TRUE)head(caffeine, n = 2)
names(caffeine)
caffeine$X0
caffeine[, 1]
caffeine["X0"]  # R knows this is a name
caffeine["0"]  # R interprets this as row 0 and expects a column number
caffeine["0",]  # R returns all values in row 0 which does not exist
caffeine["100",]  # R returns all values in row 100 which does not exist
caffeine["1",]  # R returns all values in row 1 which does exist


# See the following help pages...
?read.table
# By default check.names = TRUE, so R proceeds to check the names of the variables in the data to ensure that they are syntactically valid variable names. Calls make.names if necessary
?make.names
# A syntactically valid name consists of letters, numbers and the dot or underline characters and starts with a letter or the dot not followed by a number.
# The character "X" is prepended if necessary. All invalid characters are translated to ".". A missing value is translated to "NA". Names which match R keywords have a dot appended to them.

# What happens if check.names = FALSE? Surprisingly it works
caffeine.bad <- read.table("caffeine.txt", header = TRUE, check.names = FALSE)
head(caffeine.bad, n = 2)

names(caffeine.bad)
caffeine.bad["0"]  # R interprets this as a name
caffeine.bad["0",]  # R returns all values in row 0 which does not exist
caffeine.bad["100"] # R interprets this as a name
caffeine.bad["100",]  # R returns all values in row 100 which does not exist
caffeine.bab["1",]
caffeine.bad$100
caffeine.bad$`100`