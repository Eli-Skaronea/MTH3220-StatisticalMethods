paint_data <- read.table("PaintBrand_vs_RollerBrand.txt", header = TRUE)
paint_data[, "Roller"] <- factor(paint_data[,"Roller"])
paint_data[,"Paint"] <- factor(paint_data[,"Paint"])
attach(paint_data)
paint_aov <- aov(Coverage ~ Paint + Roller)
summary(paint_aov)
paint_residuals <- paint_aov$residuals
paint_fit <- paint_aov$fitted.values
qqnorm(paint_residuals)
qqline(paint_residuals)
names(paint_aov)
plot(paint_fit, paint_residuals)
abline(h = 0)
TukeyHSD(paint_aov)
sort(tapply(Coverage, Paint,mean))
detach(paint_data)

pf(1.37, 3, 5, lower.tail = FALSE)

pen_data <- read.table("PenSurface_vs_Brand.txt", header = TRUE)
pen_data[,"brand"] <- factor(pen_data[,"brand"])
pen_data[,"surface"] <- factor(pen_data[,"surface"])
attach(pen_data)
pen_aov <- aov(lifetime ~ brand + surface)
summary(pen_aov)
pen_residuals <- pen_aov$residuals
pen_fit <- pen_aov$fitted.values
qqnorm(pen_residuals)
qqline(pen_residuals)
plot(pen_fit, pen_residuals)
abline( h = 0)
detach(pen_data)

sales_data <- read.table("sales_based_on_shelf_space.txt", header = TRUE)
sales_data[,"week"] <- factor(sales_data[,"week"])
sales_data[,"store"] <- factor(sales_data[,"store"])
attach(sales_data)
sales_aov <- aov(sales ~ week + store + factor(space))
summary(sales_aov)
sales_res <- sales_aov$residuals
qqnorm(sales_res)
qqline(sales_res)
plot(sales_aov$fitted.values, sales_res)
abline(h = 0)
detach(sales_data)



library(lattice)
explosives_data <- read.table("explosives.txt", header = TRUE)
for(i in 1:3){
  explosives_data[,i] <- factor(explosives_data[,i])
}
attach(explosives_data)
table(explosives_data[, 1:3])
explosives_aov = aov(fragmentation ~ diameter * material * airgap)
summary(explosives_aov)
xyplot(fragmentation ~ airgap | diameter , groups= material, type = "a")
explosives_res <- explosives_aov$residuals
explosives_fit <- explosives_aov$fitted.values
qqnorm(explosives_res)
qqline(explosives_res)
plot(explosives_fit, explosives_res)
abline(h = 0)
TukeyHSD(explosives_aov)[1:3]
detach(explosives_data)

getwd()
setwd("C:/Users/Eli S/Documents/R/Data")



