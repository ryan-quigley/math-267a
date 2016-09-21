# Group B problem 2
# Ian Dupzyk
# Ryan Quigley
# Sucharu Gupta

squareNumbers <- function(n) {
    # input validation
    if (!is.numeric(n)) {
        stop("the value passed must be numeric.")
    }
    if ((n %% 10) != 0) {
        stop("the value passed in must be a multiple of 10.")
    }
    if (n <= 0) {
        stop("non-positive values are not meaningful.")
    }

    # max root value will be the nearest integer for the sqrt of n
    max_n <- as.integer(sqrt(n))

    # define start, end, labels
    start <- seq(1, n-9, 10)
    end <- seq(10, n, 10)
    labels <- paste(start, "to", end)

    # define values and buckets
    x <- c(1:max_n)
    squares <- x*x
    buckets <- cut(squares, seq(0, n, 10), labels = labels)
    bucket_smry <- summary(buckets)
    empty <- bucket_smry[bucket_smry == 0]

    # print output when necessary
    if (length(empty) == 1) {
        cat("The following interval does not contain perfect squares:", fill=TRUE)
        cat(names(empty), fill=TRUE)
    } else if ( length(empty) > 1 ) {
        cat("The following intervals do not contain perfect squares:", fill=TRUE)
        cat(names(empty), sep='\n')
    } else {}
    
    # return non-zero sized groups
    grouped <- split(squares, buckets, drop=TRUE)
    return(invisible(grouped))
}