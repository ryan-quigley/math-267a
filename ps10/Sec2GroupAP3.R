# MATH 267A
# Section 2
# Group A
#  Navid Morshed
#  Han Xu
#  Ryan Quigley
# Problem 3

# a)
load("ps10p3.RData")
n <- length(trains)
s.var <- var(trains)
s.sd <- sd(trains)

# critical values
x.bar.c <- 145 + qt(0.01, df = 19)*sqrt(s.var/n)
t.q.c <- (x.bar.c - 140)/sqrt(s.var/n)
alpha.c <- pt(t.q.c, df = n - 1, lower.tail = FALSE)

# b)
t.test(trains, mu = 140, alternative = "greater")
# Conclusion: reject the null hypothesis
