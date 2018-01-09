ammonium_data <- read.table("AmmoniumRemoval_vs_Growth.txt", header = TRUE)
attach(ammonium_data)
lmfit <- lm(y ~ x)
summary(lmfit)
plot(x,y)
abline(lmfit)
detach(ammonium_data)

porosity_data <- read.table("unitWeight_vs_porosity.txt", header = TRUE)
attach(porosity_data)
lmfit <- lm(y ~ x)
plot(x, y)
abline(lmfit)
summary(lmfit)
anova(lmfit)
detach(porosity_data)

oxide_data <- read.table("oxide_of_nitrogenEmissions.txt", header = TRUE)
attach(oxide_data)
lmfit <- lm(y ~x)
plot(x,y)
abline(lmfit)
summary(lmfit)
anova(lmfit)

summary(lmfit)$coef
predict(lmfit, interval = "confidence", newdata = data.frame(x = 10), level = .95)
detach(oxide_data)

stormwater_data <- read.table("stormwater_pollutant_removal.txt", header = TRUE)
attach(stormwater_data)
lmfit <- lm(y ~ x)
plot(x,y)
abline(lmfit)
summary(lmfit)

summary(lmfit)$coef
predict(lmfit, interval = "confidence", newdata = data.frame(x = 100), level = .95)
predict(lmfit, interval = "confidence", newdata = data.frame(x = 200), level = .95)

predict(lmfit, interval = "prediction", newdata = data.frame(x = 100), level = .95)
predict(lmfit, interval = "prediction", newdata = data.frame(x = 200), level = .95)

predict(lmfit, newdata = newx)
predict(lmfit, interval = "confidence", newdata = data.frame(y = 2), level = .95)
predict(lmfit, interval = "prediction", newdata = data.frame(y = 2), level = .95)
predFrame <- data.frame(illit = seq(.7, 2.1, by = .1))
pp <- predict(lmmurder, interval = "prediction", predFrame, level = .95)
pc <- predict(lmmurder, interval = "confidence", predFrame, level = .95)
detach(stormwater_data)

tost_rbot_data <- read.table("TOST_RBOT.txt", header = TRUE)
attach(tost_rbot_data)
lmfit <- lm(y ~ x)
plot(y, x)
abline(lmfit)
summary(lmfit)
detach(tost_rbot_data)

getwd()
setwd("C:/Users/Eli S/Documents/R/Data")

