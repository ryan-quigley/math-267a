### NOTES:
# ?numeric help page: "The default method for is.numeric returns TRUE if its argument is of mode "numeric" (type "double" or type "integer")"

### I1 ###
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

### I2 ###
getDigit <- function(x, n) {
	# check if x is real
	if (!is.numeric(x)) {
		stop("x has to be a real number.", call. = TRUE)
	}
	# check if n is a positive
	if ((!is.numeric(n)) || ((n %% 1) != 0) || (n < 0)) {
		stop("n has to be a positive integer.", call. = TRUE)
	}
	
	((x*(10^n)) %/% 1) - ((x*(10^(n-1))) %/% 1)*10	
}

### I3 ###
getDigit <- function(x, n) {
	# Check if x is real
	if (!is.numeric(x)) {
		stop("x has to be a real number.")
	}
	# Check if n is a positive integer
	if ((!is.numeric(n)) || (n %% 1 != 0) || (n < 0)) {
		stop("n has to be a positive integer.")
	}
	# All checks passed; execute code to return requested digit
	# The arithmetic is easier for positive numbers. 
	# First check if x < 0; if so, make positive
	if (x < 0) {
		y <- -x
		((y * 10 ** n) %/% 1) %% 10
	} else {
		((x * 10 ** n) %/% 1) %% 10
	}
}

### AI ###
getDigit <- function(x,n) {
    
    xcheck <- c(is.numeric(x), is.integer(x))
    xcheck <- xcheck[1] + xcheck[2]
    ncheck <- c(is.numeric(n), is.integer(n))
    ncheck <- ncheck[1] + ncheck[2]
    
    if (xcheck == 0 | ncheck == 0 | n %% 1 != 0 | n <= 0) {
        stop("x has to be a real number, n has to be a positive integer.")
    }
    
    ((x * 10 ** n) %/% 1) %% 10

}

########################################################
# DO NOT INCLUDE IN FINAL SOLUTION


### POSITIVE test cases ###
for (i in 1:23) {
	gd <- getDigit(1.12345678901234561111111, n = i)
	print(gd)
}
getDigit(-1.234, n = 2) # Implementations are returning 6
getDigit(1.234, n = 1 + 0i) # ??? Should these return a result ???
getDigit(1, n = 2)
getDigit(20, n = 2)
getDigit(300, n = 2)
getDigit(5L, n = 2)
getDigit(0, n = 2)

getDigit(c(1.123, 2.456), n = c(1,2))
getDigit(c(1.123, 2.456, 3.789), n = c(1,2))
getDigit(c(1.123,2.456), n = 2)
getDigit(1.123, n = c(1,2))



### NEGATIVE test cases ###
getDigit(1.234, n = -1)
getDigit(1.234, n = "1") # AI fails because R attempts to calculate n%%1
getDigit(1.234, n = 1 + 2i) # AI fails because R attempts to perform complex operation
getDigit(1.234, n = TRUE) # I1 FAILS because logical can be interpreted as 0/1
getDigit(1.234, n = FALSE) # I1 FAILS because logical can be interpreted as 0/1


getDigit(1.234, n = 1.5)
getDigit(TRUE, n = 1)
getDigit(FALSE, n = 1)
getDigit(1 + 2i, n = 1)
getDigit("1.234", n = 1)
getDigit(list(1), n = 1)
