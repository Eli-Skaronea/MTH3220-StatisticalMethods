pidgeon_data <- c(12, 16, 17, 15, 13, 20, 17, 10)
pidgeon_data/sum(pidgeon_data)
probs <- c(1/8, 1/8, 1/8, 1/8, 1/8, 1/8, 1/8, 1/8)
chisq.test(x = pidgeon_data, p = probs)

info_storage_date <- c(4, 15, 23, 25, 38, 31, 32, 14, 10, 8)
probs <- (5.5 - abs(1:10 - 5.5))/30
probs
chisq.test(x = info_storage_date, p = probs)
info_retrival_freq <- c(4, 15, 23, 25, 38, 31, 32, 14, 10, 8)
chickens <- matrix(c(60, 27, 
                     60, 32, 
                     120, 45),
                   nrow = 3, byrow = T)

chickens
chisq.test(chickens)

student_data <- matrix(c(28, 17, 7,
                         31, 26, 10,
                         26, 19, 11),
                       nrow = 3, byrow = T)
student_data
chisq.test((student_data))

diagnostic_time <- c(30.6, 30.1, 15.6, 26.7, 27.1, 25.4, 35, 30.8, 31.9, 53.2, 12.5, 23.2, 8.8, 24.9, 30.2)
diagnostic_test <- wilcox.test(diagnostic_time, alt = "greater", mu = 30)
diagnostic_test

method_one <- c(44.85, 46.59, 47.6, 51.08, 52.2,
                56.87, 57.03, 57.07, 60.35, 60.82,
                67.3, 70.15, 70.77, 75.21, 75.28,
                76.6, 80.3, 81.23)
method_two <- c(51.95, 56.54, 57.4, 57.6, 61.16,
                39.91, 42.01, 43.58, 48.83, 49.07,
                49.48, 49.57, 49.63, 50.75, 64.55,
                65.31, 68.59, 72.4)
wilcox.test(method_one, method_two, paired = TRUE)


potato <- c(1.88, 2.6, 1.38, 4.41, 1.87, 2.89, 3.96, 2.31)
rice <- c(1.7, 3.84, 1.13, 4.97, .86, 1.93, 3.36, 2.15)
wilcox.test(potato, rice, paired = TRUE)
setwd("C:/Users/Eli S/Documents/R/Data")

soil_data <- read.table("soilconcentrationVStreatment.txt", header = TRUE)
kruskal.test(soil_data$concentration, soil_data$treatment)
