set.seed(123)
setwd("E:/EDA/Numerical summaries/")

gap <- read.csv("gapminder_unfiltered.csv", header = T)
View(gap)
gap %>% filter(year >= 2007) %>% group_by(continent) %>% summarise(mean(lifeExp, na.rm = T), median(lifeExp, na.rm = T))
  
gap %>% filter(year >= 2007) %>%
  ggplot(aes(x = year, y = lifeExp, fill = year)) + geom_boxplot() + facet_wrap(~continent)

gap %>% filter(year >= 2007) %>%
  ggplot(aes(x = continent, y = lifeExp, fill = continent)) + geom_boxplot()

gap2007 <- gap %>% filter(year >= 2007)

gap2007 %>% group_by(continent) %>% summarise(sd(lifeExp, na.rm = T), IQR(lifeExp, na.rm = T),n())

gap2007 %>%
  ggplot(aes(x = lifeExp, fill = continent)) + geom_density(alpha = 0.3)

gap2007 %>% filter(continent == "Americas") %>% summarise(mean(lifeExp, na.rm = T),sd(lifeExp, na.rm = T), median(lifeExp), IQR(lifeExp))

gap2007 %>% filter(continent == "Americas") %>%
  ggplot(aes(x = lifeExp)) + geom_density()

gap2007 %>%
  ggplot(aes(x = pop)) + geom_density()

gap2007<- gap2007 %>% mutate(log_pop = log(pop))

gap2007 %>%  
  ggplot(aes(x = log_pop)) + geom_density()

gap2007_asia <- gap2007 %>% filter(continent == "Asia") %>% mutate(is_outlier = lifeExp < 50)
View(gap2007_asia)

gap2007_asia %>% filter(!is_outlier) %>%
  ggplot(aes(x = 1, y = lifeExp)) + geom_boxplot()

plot(ChickWeight)


library(MASS)
whiteside <- whiteside

plot(whiteside)

plot(whiteside$Gas, whiteside$Temp, xlab = "Outside Temperature", ylab = "Heating gas consumption")

head(whiteside)

plot(whiteside$Insul)

cars93 <- Cars93

attach(cars93)
plot(Max.Price,Price,pch = 17,col = "red")
points(Min.Price,Price,pch = 16,col="green")
abline(a=0,b=1)
# abline(lm(Max.Price~Price))
# abline(lm(Min.Price~Price))
detach(cars93)


library(robustbase)
animals2 <- Animals2
par(mfrow = c(1,2))
attach(animals2)
plot(brain,body)
title("Original representation")
plot(brain,body,log="xy")
title("Log-log plot")
detach(animals2)


par(mfrow = c(1,2))
attach(cars93)
hist(Horsepower,main = "hist() plot")
truehist(Horsepower, main = "truehist() plot")
detach(cars93)
par(mfrow=c(1,1))

chickweight <- ChickWeight
index16 <- chickweight[which(chickweight$Time==16),]
weights <- index16$weight

truehist(weights)
lines(density(weights))


par(mfrow=c(1,2))
library(car)

qqPlot(weights)
boston <- Boston

attach(boston)
qqPlot(tax)
detach(boston)

library(insuranceData)
data("dataCar")
dt <- dataCar

par(mfrow = c(1,2))
tbl <- sort(table(dt$veh_body), decreasing = TRUE)

pie(tbl,radius = 1.0)
title("Pie Chart")
legend(tbl)
barplot(tbl,las = 2, cex.names = 0.5)
title("Bar chart")


################################################################################
# Ex 2

par(mfrow=c(1,2))
attach(boston)
plot(rad,zn)
title("Standard scatterplot")
sunflowerplot(rad,zn)
title("Sunflower plot")


par(mfrow=c(1,1))

boxplot((crim~(rad)), log = "y" ,varwidth = T,las = 1)
title("Crime rate vs. radial highway index")
detach(boston)
str(boston)
attach(mtcars)
mosaicplot(carb~cyl)
detach(mtcars)

par(mfrow=c(1,1))

#################################################################################
# Ex 3

library(corrplot)
library(aplpack)

attach(cars93)
par(mfrow=c(1,2))
boxplot(Min.Price,Max.Price)
bagplot(Min.Price,Max.Price, cex = 120*0.4/100)
abline(a=0,b=1)
detach(cars93)

UScereal <- UScereal
str(UScereal)
UScereal <- UScereal[,2:10]
numericalVars <- UScereal
corrMat <- cor(numericalVars)
corrplot(corrMat,method = "ellipse")

par(mfrow=c(1,1))

library(rpart)

treemodel <- rpart(medv~., data = boston)
plot(treemodel)
text(treemodel,cex=0.7,col="red",xpd = TRUE)

