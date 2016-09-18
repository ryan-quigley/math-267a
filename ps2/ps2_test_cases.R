### POSITIVE test cases ###
for (i in 1:23) {
	gd <- getDigit(1.12345678901234561111111, n = i)
	print(gd)
}
getDigit(-1.234, n = 2) # Implementations are returning 6
getDigit(1.234, n = 1 + 0i) # ??? Should these return a result ???
getDigit(1, n = 2)
getDigit(20, n = 2)
getDigit(300, n = 2)
getDigit(5L, n = 2)
getDigit(0, n = 2)

getDigit(c(1.123, 2.456), n = c(1,2))
getDigit(c(1.123, 2.456, 3.789), n = c(1,2))
getDigit(c(1.123,2.456), n = 2)
getDigit(1.123, n = c(1,2))



### NEGATIVE test cases ###
getDigit(1.234, n = -1)
getDigit(1.234, n = "1") # AI fails because R attempts to calculate n%%1
getDigit(1.234, n = 1 + 2i) # AI fails because R attempts to perform complex operation
getDigit(1.234, n = TRUE) # I1 FAILS because logical can be interpreted as 0/1
getDigit(1.234, n = FALSE) # I1 FAILS because logical can be interpreted as 0/1


getDigit(1.234, n = 1.5)
getDigit(TRUE, n = 1)
getDigit(FALSE, n = 1)
getDigit(1 + 2i, n = 1)
getDigit("1.234", n = 1)
getDigit(list(1), n = 1)



x <- solveQuadr(1, -3, 4)
x
y <- solveQuadr(-4, 12, -9)
y
z <- solveQuadr(2, -1, -8)
z

solveQuadr(1.0234, -3.159, 4.604)
solveQuadr(-4.0234, 12.024, -9.014)
solveQuadr(2.358, -1.0134, -8.2694)