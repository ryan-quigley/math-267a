### 2. ###
# a)
tri.num.20 <- numeric(20)
for (i in 1:20) tri.num.20[i] <- (i*(i+1))/2
tri.num.20

# b)
tri.num.20 <- ((1:20)*(2:21))/2
tri.num.20

# c) ADD COMMENTARY regarding the performance difference
tri.num <- numeric(100000)
system.time(for (i in 1:length(tri.num)) tri.num[i] <- (i*(i+1))/2)
#   user  system elapsed 
#  0.157   0.002   0.156 
system.time(tri.num <- ((1:20)*(2:21))/2)
#   user  system elapsed 
#      0       0       0 

# d)
tri.num.50 <- ((1:50)*(2:51))/2
l <- c(letters, paste(letters, letters, sep = ""))
length(l) <- length(tri.num.50)
names(tri.num.50) <- l

# e)
vowels <- c("a", "e", "i", "o", "u")
double.vowels <- paste(vowels, vowels, sep = "")
tri.num.50[names(tri.num.50) %in% c(vowels, double.vowels)]