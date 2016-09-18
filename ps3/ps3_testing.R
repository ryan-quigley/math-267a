source("Sec2GroupBP1.R")
# V3 validation
all(v3[x %% 3 == 0] == cos(x[x %% 3 == 0]))
all(v3[x %% 3 != 0] == 1/sqrt(x[x %% 3 != 0]))

source("Sec2GroupBP2.R")
tri.num.20
tri.num[100000] == 5000050000
tri.num.50

source("Sec2GroupBP3.R")
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