source("Sec2GroupBP1.R")
v1
v2
v3
# V3 validation
x <- 1:100
all(v3[x %% 3 == 0] == cos(x[x %% 3 == 0]))
all(v3[x %% 3 != 0] == 1/sqrt(x[x %% 3 != 0]))
#
v4


source("Sec2GroupBP2.R")
tri.num.20
tri.num[100000] == 5000050000
tri.num.50

source("Sec2GroupBP3.R")
# squareNumbers: positive test cases
squareNumbers(10)
r1 <- squareNumbers(10)
r1
squareNumbers(20)
r2 <- squareNumbers(20)
r2
squareNumbers(60)
r3 <- squareNumbers(60)
r3
squareNumbers(100)
r4 <- squareNumbers(100)
r4
squareNumbers(100L)

# squareNumbers: negative test cases
squareNumbers(-10)
squareNumbers(-20)
squareNumbers(-60)
squareNumbers(-100)
squareNumbers(0)
squareNumbers(101)
squareNumbers(100.001)
squareNumbers(TRUE)
squareNumbers("100")
squareNumbers(100 + 1i)