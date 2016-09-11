getDigit <- function(x, n) {
	# check if x is real
	if (!is.numeric(x)) {
		stop("x has to be a real number.", call. = TRUE)
	} else if (((n %% 1) != 0) || (n < 0)) {
		# note: I am not using the function is.integer because R stores "integers" such as 1, 2, 3,... as numeric by default
		# thus the condition would fail unless the user specifcally casts the value as an integer before or during the function call
		stop("n has to be a positive integer.", call. = TRUE)
	} else {
		((x*(10^n)) %/% 1) - ((x*(10^(n-1))) %/% 1)*10
	}	
}


# DO NOT INCLUDE IN FINAL SOLUTION
# Demonstrating that even though 1 is an integer, R stores it as a numeric
x <- 1
if (is.integer(x)) print(x)

# Conditions that need to be test
# 1) x is a real number => try entering a complex number of a logical
# 2) n must be an integer
# 3) n must be positive

# Positive test cases
for (i in 1:16) {
	gd <- getDigit(1.1234567890123456, n = i)
	print(gd)
}
getDigit(1, n = 2)
getDigit(20, n = 2)
getDigit(300, n = 2)

# Negative test cases
getDigit(1.234, n = -1)
getDigit(1.234, n = 1.5)
getDigit(TRUE, n = 1)
getDigit(1 + 2i, n = 1)
