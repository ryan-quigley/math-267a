pdf("ps8.pdf", height = 11, width = 8.5)

par(mfrow = c(2,1))
par(mar = par("mar") + c(0,1,0,0))
ppts <- seq.int(0,1, by = 0.02)
plot(qexp(ppts, rate = 1), 
	qexp(ppts, rate = 2), 
	xlim = c(0,4), ylim = c(0,4), 
	ann = FALSE, axes = FALSE,
	pch = 20, col = "violetred",
	bty = "n")
box(lty = "solid", col = "gray50")
points(qexp(ppts, rate = 1), 
	qexp(ppts, rate = 0.5), 
	xlim = c(0,4), ylim = c(0,4), 
	ann = FALSE, pch = 20, col = "slateblue2")
axis(1, at = seq.int(0,4), labels = seq.int(0,4), lwd = 0, lwd.ticks = 1, col.ticks = "gray50")
axis(2, at = seq.int(0,4), labels = seq.int(0,4), lwd = 0, lwd.ticks = 1, col.ticks = "gray50")
lines(c(0, 4), c(0, 4), lty = 3, col = "gray50")
legend(0,4, legend = c(expression(paste(theta,"= 2")), 
	expression(paste(theta,"= 0.5"))), 
	bty = "n", 
	col = c("violetred", "slateblue2"),	
	pch = 20)
title(main = "Figure 1: Problem 2", xlab = expression(paste("Quantiles of F(a; 1)")), ylab = expression(paste("Quantiles of F(a; ",theta,")")))


par(mar = par("mar") + c(0,1,0,0))
ppts <- seq.int(0,1, by = 0.02)
plot(qweibull(ppts, shape = 1, scale = 1), 
	qweibull(ppts, shape = 1, scale = 0.5), 
	xlim = c(0,4), ylim = c(0,4), 
	ann = FALSE, axes = FALSE,
	pch = 20, col = "violetred",
	bty = "n")
box(lty = "solid", col = "gray50")
points(qweibull(ppts, shape = 1, scale = 1), 
	qweibull(ppts, shape = 2, scale = 2), 
	xlim = c(0,4), ylim = c(0,4), 
	ann = FALSE, pch = 20, col = "slateblue2")
axis(1, at = seq.int(0,4), labels = seq.int(0,4), lwd = 0, lwd.ticks = 1, col.ticks = "gray50")
axis(2, at = seq.int(0,4), labels = seq.int(0,4), lwd = 0, lwd.ticks = 1, col.ticks = "gray50")
lines(c(0, 4), c(0, 4), lty = 3, col = "gray50")
legend(0,4, legend = c(expression(paste(lambda,"= 2, ", gamma,"= 1")), 
	expression(paste(lambda,"= 0.25, ",gamma,"= 2"))), 
	bty = "n", 
	col = c("violetred", "slateblue2"),	
	pch = 20)
title(main = "Figure 2: Problem 3", xlab = expression(paste("Quantiles of F(a; 1, 1)")), ylab = expression(paste("Quantiles of F(a; ",lambda,", ",gamma,")")))


dev.off()