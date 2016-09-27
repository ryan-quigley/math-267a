# Section 2
# Group C
#  Jennifer Chu
#  Samnang Tep
#  Ryan Quigley
# Problem 4

# a) 
name <- scan("ps04p4.txt", what = character(), sep = "\n")
space.rm <- gsub('\\s', '.', name)
user <- gsub('([A-Z])', '\\L\\1', space.rm, perl = TRUE)
email <- paste(user, "@ponyville.edu", sep = "")


# b)
groups <- LETTERS[1:(length(name)/3)]
rs <- sample(email, size = length(email))
name.ordered <- name[match(rs, email)]
ps1grps <- data.frame(name = name.ordered, email = rs, ps1 = groups)


# c)
write.table(ps1grps, "ps1grps.txt", quote = FALSE, sep = ",", row.names = FALSE)


# d)
ps1grps.in <- read.table("ps1grps.txt", header = TRUE, sep = ",")
identical(ps1grps, ps1grps.in)