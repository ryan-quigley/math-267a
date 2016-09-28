# Section 2
# Group C
#  Jennifer Chu
#  Samnang Tep
#  Ryan Quigley
# Problem 4

# a) 
name <- scan("ps04p4.txt", what = character(), sep = "\n")
space.rm <- gsub(' ', '.', name)
email <- paste(tolower(user), "@ponyville.edu", sep = "")


# b)
groups <- rep(LETTERS[1:(length(name)/3)], each = 3)
rs <- sample(groups)
ps1grps <- data.frame(name = name, email = email, ps1 = rs)


# c)
# col.names = TRUE by default
write.table(ps1grps, "ps1grps.txt", quote = FALSE, row.names = FALSE, sep = ",") 


# d)
ps1grps.in <- read.table("ps1grps.txt", header = TRUE, sep = ",")
identical(ps1grps, ps1grps.in)