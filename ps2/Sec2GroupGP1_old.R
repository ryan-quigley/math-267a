solveQuadr <- function(a = 1, b, c) {
	# d: discriminant
	d = b^2 - 4*a*c
	
	# check sign of discriminant
	if (d < 0) {
		cat("There are no real roots.")
		invisible(0)
	} else if (d > 0) {
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

solveQuadr <- function(a = 1, b, c) {
	# d: discriminant
	d = b^2 - 4*a*c
	
	# check sign of discriminant
	if (d < 0) {
		cat("There are no real roots.")
		invisible(0)
	} 
	if (d > 0) {
		r1 = (-b - d^(0.5))/(2*a)
		r2 = (-b + d^(0.5))/(2*a)
		cat("1st root: ", r1, "\n", "2nd root: ", r2, sep = "")
		invisible(1)
	}
	if (d == 0) {
		r = (-b)/(2*a)
		cat("root:", r)
		invisible(1)
	}
}

solveQuadr <- function(a=1,b,c) { 
    
    descrim <- b^2 - 4*a*c 
    
    if (descrim < 0){
        cat("There are no real roots.")
        invisible(0)
        }
    
    else if (descrim == 0) {
        root <- -b/(2*a)
        cat("root:", root)
        invisible(1)
        }
    
    else {
        root1 <- (-b + sqrt(descrim))/(2*a)
        root2 <- (-b - sqrt(descrim))/(2*a)
        cat("root1: ", root1, "\n", "root2: ", root2, sep="")
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

solveQuadr(1.0234, -3.159, 4.604)
solveQuadr(-4.0234, 12.024, -9.014)
solveQuadr(2.358, -1.0134, -8.2694)