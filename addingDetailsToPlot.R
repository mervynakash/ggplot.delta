set.seed(123)
setwd("E:/EDA/Programs/")

par()

names(par())
length(par())

library(robustbase)

par(mfrow = c(2,2))
attach(Animals2)
plot(brain,type = "p")
title("points")
plot(brain, type = "l")
title("lines")
plot(brain,type = "o")
title("overlaid")
plot(brain,type = "s")
title("steps")
par(mfrow=c(1,1))
detach(Animals2)

library(MASS)

max.hp <- max(Cars93$Horsepower,mtcars$hp)

max.mpg <- max(Cars93$MPG.city,Cars93$MPG.highway,mtcars$mpg)

plot(max.hp,max.mpg,type = "n", xlab = "Horsepower", ylab = "Miles per gallon", xlim = c(0,max.hp), ylim = c(0,max.mpg))
points(mtcars$hp,mtcars$mpg, pch = 1, col = "green")
points(Cars93$Horsepower,Cars93$MPG.city, pch = 15, col = "brown")
points(Cars93$Horsepower, Cars93$MPG.highway, pch = 2, col = "blue")

# Ex 2

x <- seq(0,10,length = 200)

gauss1 <- dnorm(x, mean = 2, sd = 0.2, log = FALSE)
gauss2 <- dnorm(x, mean = 4, sd = 0.5, log = FALSE)

plot(x,gauss1, type = "l", ylab = "Gaussian Probability Density")
lines(x,gauss2, lwd = 3, lty = 2)

plot(mtcars$hp,mtcars$mpg,type = "n", xlab = "Horsepower", ylab = "Gas Mileage")
points(mtcars$hp, mtcars$mpg, pch = as.numeric(mtcars$cyl))
points(mtcars$hp, mtcars$mpg, pch = as.character(mtcars$cyl))

lmao <- lm(Gas~Temp, data = whiteside)
plot(whiteside$Temp,whiteside$Gas, xlab = "Temperature", ylab = "Gas")
abline(lmao,lty = 2)

# Ex 3

plot(Cars93$Horsepower,Cars93$MPG.city, pch = 15)
index3 <- which(Cars93$Cylinders == 3)
text(x = Cars93$Horsepower[index3], y = Cars93$MPG.city[index3] ,label = Cars93$Make[index3], adj = 0)


plot(Cars93$Horsepower, Cars93$MPG.city, pch = 16, xlab = "Horsepower", ylab = "MPG City")
points(Cars93$Horsepower[index3], Cars93$MPG.city[index3], type = "o")
text(x = Cars93$Horsepower[index3], y = Cars93$MPG.city[index3] ,label = Cars93$Make[index3],adj = -0.2, cex = 0.8, font = 4, srt = -30)


plot(whiteside$Temp, whiteside$Gas, pch = 17, xlab = "Temperature", ylab = "Gas")
indexB <- which(whiteside$Insul %in% "Before")
indexA <- which(whiteside$Insul %in% "After")
text(whiteside$Temp[indexB],whiteside$Gas[indexB],col = "blue", label = whiteside$Insul[indexB], srt = 30, cex = 0.8)
text(whiteside$Temp[indexA],whiteside$Gas[indexA],col = "red", label = whiteside$Insul[indexA], srt = -20, cex = 0.8)

# Ex 4
attach(whiteside)
plot(Temp,Gas,type = "n", xlab = "Outside Temperature", ylab = "Heating gas consumption")
indexB <- which(whiteside$Insul %in% "Before")
indexA <- which(whiteside$Insul %in% "After")
points(Temp[indexB],Gas[indexB],pch = 17, col = "blue")
points(Temp[indexA],Gas[indexA],pch = 16, col = "red")
legend("topright", pch = c(17,16) ,legend = c("Before","After"), col = c("blue","red"))
detach(whiteside)

attach(UScereal)
boxplot(sugars~shelf, axes = FALSE)
axis(side = 2, at = (sugars))
axis(side = 1, at = (shelf))
axis(side = 3, at = unique(shelf), labels = c("floor","middle","top"))
detach(UScereal)

attach(Cars93)
plot(Horsepower,MPG.city)
trend1 <- supsmu(Horsepower,MPG.city, bass = 1)
lines(trend1)
trend2 <- supsmu(Horsepower,MPG.city, bass = 10)
lines(trend2, lty = 3)
detach(Cars93)


