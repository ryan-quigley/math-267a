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
		warning("NAs removed from x, sample size reduced.")
	}
	n <- length(a[!is.na(a)])
	lambda.hat <- 1/mean(x, na.rm = TRUE)
	chi.sq.q <- qchisq(p = c(1 - alpha/2, alpha/2), df = 2*n)
	ci <- round((2*n)/(lambda.hat*chi.sq.q), 2)
	names(ci) <- c("lcl", "ucl")
	ci
}

#a <- c(7.44, 7.69, 0.96, 0.27, 2.03, NA, 1.38)
#ciExp(a, alpha = 0.05)

# b)
arr.time <- c(28.62, 2.14, 8.14, 2.17, 5.72, 10.64, 6.89, 32.09, 19.37, 3.07)
arrival.ci <- ciExp(arr.time, alpha = 0.02)

# c) In order to guarantee that the confidence interval contains the true value for the average time between events, the interval must be the entire support of the distribution: the support of the $\chi^2$ distribution is [0, \infty)
ciExp(arr.time, 0)