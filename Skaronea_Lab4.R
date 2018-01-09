memory_data <- read.table("memory.txt", header = TRUE)
muscles_data <- read.table("muscles.txt", header = TRUE)
attach(memory_data)
memaov1 <- aov(Score ~ Reinforcement + Isolation)
summary(memaov1)
library(lattice)
xyplot(Score ~ Isolation, groups = Reinforcement, type = "a")
memaov2 <- aov(Score ~ Reinforcement * Isolation)
summary(memaov2)
memres <- memaov2$residuals
memfit <- memaov2$fitted.values
qqnorm(memres)
qqline(memres)
plot(memfit, memres)
abline(h = 0)

pdf("Lab4_Memory.pdf")
xyplot(Score ~ Isolation, groups = Reinforcement, type = "a", main = "Interaction plot")
qqnorm(memres)
qqline(memres)
plot(memfit, memres, main = "Residuals vs Fitted Values")
abline(h = 0)
dev.off()

detach(memory_data)
muscles <- read.table("muscles.txt", header = TRUE)
attach(muscles)
str(muscles)
PctCh <- (muscles$NormalWt - muscles$DenervedWt) / muscles$NormalWt
muscles$PctCh <- PctCh
xyplot(PctCh ~ NumberOfTrts | TypeOfCurrent, groups = TrtLength, type = "a", main = "Interaction plots for each Type of Current")
musclesaov <- aov(PctCh ~ NumberOfTrts * TrtLength * TypeOfCurrent)
summary(musclesaov)
pdf("Lab4_Muscles.pdf")
xyplot(PctCh ~ NumberOfTrts | TypeOfCurrent, groups = TrtLength, type = "a", main = "Interaction plots for each Type of Current")
dev.off()
detach(muscles)









