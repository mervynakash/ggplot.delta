set.seed(123)
setwd("E:/EDA/Programs/")

library(MASS)

# Ex 1

ncol(Cars93)

plot(Cars93)

colnames(UScereal)

keep_vars <- c("calories","protein","fat","fibre","carbo","sugars")

df <- UScereal[,keep_vars]

par(mfrow=c(2,2))

matplot(df$calories,df[,c("protein","fat")], xlab = "calories", ylab = "")
title("Two Scatterplot")
matplot(df$calories,df[,c("protein","fat","fibre")], xlab = "calories", ylab = "")
title("Three Scatterplot")
matplot(df$calories,df[,c("protein","fat","fibre","carbo")], xlab = "calories", ylab = "")
title("Four Scatterplot")
matplot(df$calories,df[,c("protein","fat","fibre","carbo","sugars")], xlab = "calories", ylab = "")
title("Five Scatterplot")

par(mfrow=c(1,1))

library(wordcloud)
library(tm)

mfr_table <- table(Cars93$Manufacturer)
wordcloud(word = Cars93$Manufacturer,scale = c(2,0.25))
wordcloud(word = names(mfr_table), freq = as.numeric(mfr_table), scale = c(2,0.25))

wordcloud(word = names(mfr_table), freq = as.numeric(mfr_table), scale = c(2,0.25), min.freq = 1)

model_table <- table(Cars93$Model)
wordcloud(word = names(model_table), freq = as.numeric(model_table), scale = c(0.75,0.25), min.freq = 1)


# Ex 2

par(mfrow=c(2,2))
attach(anscombe)
plot(x1,y1)
plot(x2,y2)
plot(x3,y3)
plot(x4,y4)

par(mfrow=c(1,1))

max_x <- max(x1,x2,x3,x4)
min_x <- min(x1,x2,x3,x4)
max_y <- round(max(y1,y2,y3,y4),0)
min_y <- round(min(y1,y2,y3,y4),0)

min_x <- range(x1,x2,x3,x4)[1]
max_x <- range(x1,x2,x3,x4)[2]


par(mfrow=c(2,2))
plot(x1,y1,xlim = c(min_x,max_x), ylim = c(min_y,max_y), xlab = "x value", ylab = "y value")
title("First Dataset")
plot(x2,y2,xlim = c(min_x,max_x), ylim = c(min_y,max_y), xlab = "x value", ylab = "y value")
title("Second Dataset")
plot(x3,y3,xlim = c(min_x,max_x), ylim = c(min_y,max_y), xlab = "x value", ylab = "y value")
title("Third Dataset")
plot(x4,y4,xlim = c(min_x,max_x), ylim = c(min_y,max_y), xlab = "x value", ylab = "y value")
title("Fourth Dataset")

detach(anscombe)

library(car)

par(mfrow = c(2,2))

plot(geyser$duration) 
title("Raw data")

truehist(geyser$duration)
title("Histogram")

plot(density(geyser$duration))
title(density)

qqPlot(geyser$duration, main = "QQ Plot") 


#Ex 3

layoutmatrix <- matrix(c(0,1,2,0,0,3),nrow=3, byrow = T)

layout(layoutmatrix)
layout.show(n = 3)

layout(layoutmatrix) 
indexB <- whiteside[whiteside$Insul == "Before",]
indexA <- whiteside[whiteside$Insul == "After",]

plot(indexB$Temp,indexB$Gas,ylim=c(0,8))
plot(whiteside$Temp,whiteside$Gas, ylim = c(0,8))
plot(indexA$Temp,indexA$Gas,ylim=c(0,8))

row1 <- c(1,0,0)
row2 <- c(0,2,2)
layoutVector <- c(row1,row2,row2)
layoutmatrix <- matrix(layoutVector, nrow = 3, byrow = T)
layout(layoutmatrix)

plot(Boston$rad,Boston$zn)
sunflowerplot(Boston$rad,Boston$zn)
