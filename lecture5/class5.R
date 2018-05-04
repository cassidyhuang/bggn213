#' ---
#' title: "Bioinformatics class 5"
#' author: "Cassidy"
#' date: "April 18, 2018"
#' ---

# Bioinformatics Class 5
# Plots

x <- rnorm(1000,0)

summary(x)

# see this data as a graph
boxplot(x)

# Good old histogram
hist(x)


# Section 1A from work sheet
baby <- read.table("bggn213_05_rstats/weight_chart.txt", header = TRUE)

plot(baby, type="b", lty=2, pch=1:9, cex=1, lwd=3, ylim=c(3,10), xlim=c(0,10), xlab="Age (months)", ylab="Weight (kg)", main="Baby weight")

# Section 1B
feat <- read.table("bggn213_05_rstats/feature_counts.txt", sep ="\t", header=TRUE)
par(mar=c(5,11,4,2))
barplot(feat$Count, names.arg=feat$Feature, horiz = TRUE, ylab="Count", main="Features", las=1)


# Section 2A
#read.table("bggn213_05_rstats/male_female_counts.txt", header=TRUE, sep="\t", row.names = NULL)
MFcount <- read.delim("bggn213_05_rstats/male_female_counts.txt")
mycols <- cm.colors(nrow(MFcount))
barplot(MFcount$Count, col=mycols)


barplot(MFcount$Count, col=mycols)

# Section 2B
updown <- read.table("bggn213_05_rstats/up_down_expression.txt", header=TRUE)
plot(updown$Condition1, updown$Condition2, col=updown$State)

table(updown$State)
palette(c("red","green","blue"))
plot(updown$Condition1, updown$Condition2, col=updown$State, xlab = "Condition1", ylab = "Condition2")




