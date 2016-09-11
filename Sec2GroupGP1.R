solveQuadr <- function(a = 1, b, c) {
	# d: discriminant
	d = b^2 - 4*a*c
	
	# check sign of discriminant
	if (d < 0) {
		cat("There are no real roots.")
		invisible(0)
	} else if (d > 0){
		r1 = (-b - d^(0.5))/(2*a)
		r2 = (-b + d^(0.5))/(2*a)
		cat("1st root: ", r1, "\n", "2nd root: ", r2, sep = "")
		invisible(1)
	} else {
		r = (-b)/(2*a)
		cat("root:", r)
		invisible(1)
	}
}

# DO NOT INCLUDE IN FINAL ANSWER
# Code for testing the return value and print output
x <- solveQuadr(1, -3, 4)
x
y <- solveQuadr(-4, 12, -9)
y
z <- solveQuadr(2, -1, -8)
z