### 1. ###
# v1
v1_1 <- 1:100
v1_2 <- c(1,-1)
v1 <- v1_1*v1_2

# v2
v2 <- ep(1:99, rep(1:3, times = 33))

# v3
x <- 1:100
v3 <- ifelse(x %% 3 == 0, cos(x), 1/sqrt(x))
# check
v3[x %% 3 == 0] == cos(x[x %% 3 == 0]) # PASSED
v3[x %% 3 != 0] == 1/sqrt(x[x %% 3 != 0]) # PASSED

# v4
length(v1) == length(v3)
v4 <- rep(100, "C")
v4[v1 > 0] <- "A"
v4[(v1 < 0) & ((0 < v3) & (v3 < 0.138))] <- "B"
v4 <- factor(v4)
############################################
# DR. LEE: Should this be a factor or is a character vector ok
############################################



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



### 3. ###
squareNumbers <- function(n) {
	if (!is.numeric(n) || (n %% 10 != 0)) {
		stop("n must be an integer that is a multiple of 10")
	}
	x <- 1:n
	squares <- x[sqrt(x) %% 1 == 0]
	lower.bound <- seq.int(from = 1, to = (n - 9), by = 10)
	upper.bound <- seq.int(from = 10, to = n, by = 10)
	names <- paste(lower.bound, " to ", upper.bound, sep = "")
	groups <- cut(squares, breaks = c(0, upper.bound), labels = names)
	counts <- summary(groups)
	zeros <- s[s == 0]
	if (length(zeros) > 1) {
		cat("The following intervals do not contain perfect squares:\n", paste(names(zeros),"\n", sep = ""), sep = "")
	} else if (length(zeros) == 1) {
		cat("The following interval does not contain perfect squares:\n", names(zeros), sep = "")
	}
	invisible(split(squares, groups, drop = TRUE))
}