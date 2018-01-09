blood_data <- read.table("blood.txt", header = TRUE)
blood_data[3, 1]
blood_data[3,]
blood_data[,1]
blood_data$Diet == "A"

Adata <- blood_data[blood_data$Diet == "A", ]
Adata
Bdata <- blood_data[blood_data$Diet == "B", ]
Cdata <- blood_data[blood_data$Diet == "C", ]
Ddata <- blood_data[blood_data$Diet == "D", ]

ybarA = mean(Adata[, "CoagTime"])
ybarB = mean(Bdata[, "CoagTime"])
ybarC = mean(Cdata[, "CoagTime"])
ybarD = mean(Ddata[, "CoagTime"])
ybars <- c(ybarA, ybarB, ybarC, ybarD)
names(ybars) <- c("ybarA", "ybarB", "ybarC", "ybarD")
sort(ybars)
blood_anova <- aov(CoagTime ~ Diet, data = blood_data)
summary(blood_anova)
pdf("Blood_Data.pdf")
boxplot(CoagTime ~ Diet, data = blood_data, xlab = "Diet", 
        ylab = "Coagulation Time", main = "Coagulation Time vs Diet", col = "Brown")
hist(blood_anova$residuals, xlab= "Residuals", 
     ylab= "Frequency", main="Histogram of Residuals", col = "Red")
qqnorm(blood_anova$residuals, xlab = "Theoretical Quantiles", 
       ylab = "Sample Quantities", main = "Normal Q-Q Plot")
qqline(blood_anova$residuals, col = "blue")
dev.off()
names(blood_anova)
TukeyHSD(blood_anova)
