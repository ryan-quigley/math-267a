### 3. ###
# Final Code: Ian

squareNumbers <- function(n) {
	if (!is.numeric(n) || (n %% 10 != 0)) {
		stop("n must be an integer that is a multiple of 10.")
	} else if (n < 0) {
		stop("there are no negative perfect squares.")
	}
	squares <- (1:sqrt(n))^2
	lower.bound <- seq.int(from = 1, to = (n - 9), by = 10)
	upper.bound <- seq.int(from = 10, to = n, by = 10)
	names <- paste(lower.bound, " to ", upper.bound, sep = "")
	groups <- cut(squares, breaks = c(0, upper.bound), labels = names)
	counts <- summary(groups)
	zeros <- counts[counts == 0]
	if (length(zeros) > 1) {
		cat("The following intervals do not contain perfect squares:\n", paste(names(zeros),"\n", sep = ""), sep = "")
	} else if (length(zeros) == 1) {
		cat("The following interval does not contain perfect squares:\n", names(zeros), sep = "")
	}
	invisible(split(squares, groups, drop = TRUE))
}
