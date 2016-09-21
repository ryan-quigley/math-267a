### 2. ###
# a)
# Final Code: Ian
tri.num.20 <- numeric(20)
for (i in 1:20) tri.num.20[i] <- (i*(i+1))/2
tri.num.20


# b)
# Final Code: Ian
tri.num.20 <- (1:20)*((2:21)/2)
tri.num.20


# c)
# Final Code: Ian
# ADD COMMENTARY regarding the performance difference

# Check to make sure each method is giving the same result
tri.num1 <- numeric(100000)
for (i in 1:100000) tri.num1[i] <- (i*(i+1))/2
tri.num2 <- numeric(100000)
tri.num2 <- (1:100000)*((2:100001)/2)
all(tri.num1 == tri.num2)

# Time the two methods
tri.num <- numeric(100000)
system.time(for (i in 1:100000) tri.num[i] <- (i*(i+1))/2)
#   user  system elapsed 
#  0.191   0.004   0.204 
tri.num <- numeric(100000)
system.time(tri.num <- (1:100000)*((2:100001)/2))
#   user  system elapsed 
#  0.001   0.000   0.000 


# d)
# Final Code: Ian
tri.num.50 <- ((1:50)*(2:51))/2
l <- c(letters, paste(letters, letters, sep = ""))
length(l) <- length(tri.num.50)
names(tri.num.50) <- l


# e)
# Final Code: Ian
vowels <- c("a", "e", "i", "o", "u")
double.vowels <- paste(vowels, vowels, sep = "")
tri.num.50[names(tri.num.50) %in% c(vowels, double.vowels)]