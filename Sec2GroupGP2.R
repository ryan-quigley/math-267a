getDigit <- function(x, n) {
	# According to the is.numeric help page: is.numeric returns TRUE if its argument is of mode "numeric" (type "double" or type "integer")
	
	# Check if x is real
	if (!is.numeric(x)) {
		stop("x has to be a real number.")
	}
	
	# Check if n is a positive integer
	if ((!is.numeric(n)) || (n %% 1 != 0) || (n < 0)) {
		stop("n has to be a positive integer.")
	}
	
	# Check if x is negative; if so, make positive because the arithmetic is easier
	if (x < 0) {
		y <- -x
		((y * 10 ** n) %/% 1) %% 10
	} else {
		((x * 10 ** n) %/% 1) %% 10
	}
}