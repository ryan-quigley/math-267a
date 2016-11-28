# MATH 267A
# Section 2
# Group A
#  Navid Morshed
#  Han Xu
#  Ryan Quigley
# Problem 2

# a)

ciExp <- function(x, alpha) {
	if (!is.numeric(x)) {
		stop("x must be a numeric vector")
	}
	if (alpha < 0 | alpha > 1) {
		stop("alpha must be in the interval (0, 1)")
	}
	if (any(is.na(x))) {
		warning(paste(sum(is.na(x)), "NAs removed from x, sample size reduced."))
	}
	n <- length(x[!is.na(x)])
	lambda.hat <- 1/mean(x, na.rm = TRUE)
	chi.sq.q <- qchisq(p = c(1 - alpha/2, alpha/2), df = 2*n)
	ci <- round((2*n)/(lambda.hat*chi.sq.q), 2)
	names(ci) <- c("lcl", "ucl")
	ci
}

# Function validation:
# a <- c(7.44, 7.69, 0.96, 0.27, 2.03, NA, 1.38)
# ciExp(a, alpha = 0.05)

# b)
arv.time <- c(28.62, 2.14, 8.14, 2.17, 5.72, 10.64, 6.89, 32.09, 19.37, 3.07)
arrival.ci <- ciExp(arv.time, alpha = 0.02)
