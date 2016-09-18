### 3. ###
# Do we need to handle the case where n is negative?
# There are NO negative perfect squares because a perfect square is a number that is the square of an integer

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
	zeros <- counts[counts == 0]
	if (length(zeros) > 1) {
		cat("The following intervals do not contain perfect squares:\n", paste(names(zeros),"\n", sep = ""), sep = "")
	} else if (length(zeros) == 1) {
		cat("The following interval does not contain perfect squares:\n", names(zeros), sep = "")
	}
	invisible(split(squares, groups, drop = TRUE))
}
