beer_data <- read.table("beer.txt", header = TRUE)
attach(beer_data)
str(beer_data)
beer_data
pdf("plot1.pdf")
par(mfrow = c(2, 2))
boxplot((time ~ angle), main = "Angle vs Time", col = "skyblue", 
        ylab = "Time", xlab = "Angle")
boxplot((time ~ deter), main = "Deter vs Time", col = "yellow",
        ylab = "Time", xlab = "Deter")
boxplot((time ~ chill), main = "Chill vs Time", col = "red",
        ylab = "Time", xlab = "Chill")
boxplot((time ~ botcan), main = "Bot/Can vs Time", col = "green",
        ylab = "Time", xlab = "Bot/Can")
dev.off()
myvars <- tapply(time, angle, var)
f <- myvars[1]/myvars[2]
f
pf(f,7,7) * 2
t.test(time~angle)
summary(aov(time~angle))
