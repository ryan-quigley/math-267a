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

# Symbols
# $H_0: \mu = 140$
# $H_a: \mu > 140$
# Words
# The null hypothesis is that the previous survey results are still accurate and relevant
# The alternative hypothesis is that the old survey results are outdated and logging cars are actually being loaded more heavily in recent years.
# Rejection region: in this context type II error is a very serious problem. If we fail to reject the null hypothesis when in reality the average weight is greater than 140, then the engineering corps will not build the trestle to higher tolerances and the trestle will be at greater risk of structural failure. We can control type II error by making the rejection region larger, which can be achieved by choosing a lower bound for the region that is very close to 140. Determining that particular value should be left to the engineers, but for the sake of proceeding with the test we calculate it as follows. Say at a value of 150 for the true average weight we want to make sure the type II error is at most 1%:

x.bar.c <- 150 + qt(0.01, df = 19)*sqrt(s.var/n)
t.q.c <- (x.bar.c - 140)/sqrt(s.var/n)
alpha.c <- pt(t.q.c, df = n - 1, lower.tail = FALSE)
# We want to determine whether or not to build the trestle to higher tolerances to accommodate heavier cars, so we are really only interested in the case where the average weight is greater than the hypothesized value.

# b)
t.test(trains, mu = 140, alternative = "greater")
# reject the null hypothesis

# c) the t-test from part (b) only allows us to advise the engineers to build the trestle to higher tolerance, it does not allow us to suggest a minimum threshold. [We would need a confidence interval for this]

# d)