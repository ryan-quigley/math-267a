# Section 2
# Group C
#  Qiaoqiao Jiang
#  Yunyun Tao
#  Ryan Quigley
# Problem 1

# Dataset
data(Cars93, package = "MASS")

# Histogram
par(mar = c(5,5,3,1), bg = "black")
hist(Cars93$Price, 
	xlim = c(0,70), 
	col = "hotpink", 
	border = "cyan", 
	ann = FALSE, 
	axes = FALSE)

# X-axis
axis(1, 
	at = seq.int(0, 70, by = 10), 
	labels = seq.int(0, 70, by = 10), 
	col = "limegreen", 
	col.ticks = "orangered",
	col.axis = "whitesmoke",
	family = "sans", 
	font.axis = 2, 
	cex.axis = 0.75)

# Y-axis: major ticks
axis(2, 
	at = seq.int(0, 30, by = 10), 
	labels = seq.int(0, 30, by = 10), 
	col = "purple", 
	col.ticks = "aquamarine", 
	lwd = 2, 
	lwd.ticks = 2,
	family = "serif", 
	font.axis = 4, 
	cex.axis = 2, 
	col.axis = "tomato", 
	las = 1)
# Y-axis: major ticks (inside)
axis(2, 
	at = seq.int(0, 30, by = 10), 
	labels = NA, col = "transparent", 
	col.ticks = "aquamarine", 
	lwd.ticks = 2, 
	tcl = -par("tcl"))

# Y-axis: minor ticks
axis(2, 
	at = c(15, 25), 
	labels = c(15, 25), 
	col = "transparent", 
	col.ticks = "gold", 
	lwd.ticks = 2, 
	tcl = -par("tcl"),
	family = "sans", 
	font.axis = 2, 
	cex.axis = 1, 
	col.axis = "gold", 
	las = 0, 
	mgp = c(3,-2,0))

# Main title
title(main = "Gaudy", 
	col.main = "blue", 
	font.main = 2, 
	family = "mono", 
	cex.main = 3)
# X-axis label
title(xlab = "Horizontal axis", 
	col.lab = "yellow", 
	font.lab = 3, 
	family = "sans", 
	cex.lab = 2, 
	adj = 1)
# Y-axis label
title(ylab = "Vertical axis", 
	col.lab = "skyblue", 
	family = "sans", 
	cex.lab = 1.5)

# Text overlay
text(x = 25, y = 15, 
	labels = "Histogram", 
	col = "blue", 
	cex = 3, 
	srt = 40, 
	family = "mono")
