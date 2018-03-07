set.seed(123)
setwd("E:/EDA/Programs/")

library(ggplot2)
library(dplyr)
library(bindrcpp)
library(tidyr)

####################################################################################

# Ex 1

str(mtcars)

mtcars %>% ggplot(aes(x = cyl, y = mpg)) + geom_point()

mtcars %>% ggplot(aes(x = as.factor(cyl), y = mpg)) + geom_point() 

####################################################################################

# Ex 2

mtcars %>% ggplot(aes(x = wt, y = mpg)) + 
  geom_point()

mtcars %>%
  ggplot(aes(x = wt, y = mpg, color = disp)) + 
  geom_point()

mtcars %>%
  ggplot(aes(x = wt, y = mpg, color = disp, size = disp)) + 
  geom_point()

mtcars %>% 
  ggplot(aes(x = wt, y = mpg, shape = disp)) + 
  geom_point()

####################################################################################

# Ex 3

str(diamonds)

diamonds %>% 
  ggplot(aes(x = carat, y = price)) + 
  geom_point() + 
  geom_smooth()

diamonds %>% 
  ggplot(aes(x = carat, y = price)) + 
  geom_jitter(alpha = 0.2) 

diamonds %>% 
  ggplot(aes(x = carat, y = price)) + 
  geom_smooth(aes(color = clarity))

diamonds %>% 
  ggplot(aes(x = carat, y = price, color = clarity)) + 
  geom_point(alpha = 0.4) 

dia_plot <- diamonds %>%
              ggplot(aes(x = carat, y = price))

dia_plot + geom_point(aes(color = clarity))

dia_plot + geom_point(alpha = 0.2)

dia_plot + geom_smooth(se = F)

dia_plot + geom_smooth(aes(color = clarity), se = F)

####################################################################################

#####################
# DATA
#####################

# Ex 1

attach(mtcars)
plot(wt,mpg, col = cyl)
detach(mtcars)

fcyl1 <- factor(mtcars$cyl, levels = unique(mtcars$cyl))
mtcars <- cbind(mtcars,"fcyl" = fcyl1)

# mtcars$fcyl <- as.factor(mtcars$cyl)

attach(mtcars)
plot(wt,mpg, col = fcyl1)
detach(mtcars)

carMOdel <- lm(mpg~wt, mtcars)
attach(mtcars)
plot(wt,mpg, col = fcyl1)
abline(carMOdel, lty = 2)
legend("topright",col = as.character(unique(fcyl1)), legend = as.character(unique(fcyl1)), pch = 1)
legend("topright",col = levels(fcyl1), legend = levels(fcyl1), pch = 1)
detach(mtcars)

mtcars %>%
  ggplot(aes(x = wt, y = mpg, color = fcyl1)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = F) + 
  geom_smooth(aes(group = 1), method = "lm", se = F, lty = 2)

####################################################################################

# Ex 2

iris.tidy <- gather(iris, key = Part, value = Value, -Species)
iris.tidy <- separate(iris.tidy, Part, c("Part","Measure"))
View(iris.tidy)

iris.wide <- iris %>% mutate(Flower = 1:nrow(iris))
head(iris.wide)
iris.wide <- gather(iris.wide, Part, Value, -c(Species, Flower))
iris.wide <- separate(iris.wide, Part, c("Part","Measure"))
iris.wide <- spread(iris.wide,Measure, Value)
head(iris.wide)
iris.wide$Flower <- NULL

iris.tidy %>% 
  ggplot(aes(x = Species, y = Value, color = Measure)) + 
  geom_jitter()

iris.wide %>% 
  ggplot(aes(x = Length, y = Width, color = Part)) + 
  geom_jitter() + 
  facet_wrap(~Species)

####################################################################################
 
#############
# AESTHETICS
#############

# Ex 1

mtcars$cyl <- as.factor(mtcars$cyl)

mtcars %>% 
  ggplot() + 
  geom_point(aes(x = mpg, y = cyl))

mtcars %>% 
  ggplot() + 
  geom_point(aes(x = wt, y = mpg))

mtcars %>% 
  ggplot() + 
  geom_point(aes(x = wt, y = mpg, col = cyl))

mtcars %>% 
  ggplot() + 
  geom_point(aes(x = wt, y = mpg, col = cyl, size = 4), shape = 1)

mtcars %>% 
  ggplot() + 
  geom_point(aes(x = wt, y = mpg, fill = cyl, size = 4), shape = 1)

mtcars %>% 
  ggplot() + 
  geom_point(aes(x = wt, y = mpg, fill = cyl, size = 4), shape = 21, alpha = 0.6)

mtcars %>% 
  ggplot() + 
  geom_point(aes(x = wt, y = mpg, col = am, size = 4), shape = 21, alpha = 0.6)


mtcars %>% 
  ggplot(aes(x = wt, y = mpg)) + 
  geom_point(size = cyl)

mtcars %>% 
  ggplot(aes(x = wt, y = mpg)) + 
  geom_point(aes(alpha = cyl))

mtcars %>% 
  ggplot(aes(x = wt, y = mpg)) + 
  geom_point(shape = cyl)

mtcars %>% 
  ggplot(aes(x = wt, y = mpg)) + 
  geom_point() + 
  geom_text(label = cyl, nudge_x = 0.1)

mycolor <- "#4ABEFF"

mtcars %>% 
  ggplot(aes(x = wt, y = mpg, col = cyl)) + 
  geom_point() 

mtcars %>% 
  ggplot(aes(x = wt, y = mpg, col = "#4ABEFF")) + 
  geom_point()

mtcars %>% 
  ggplot(aes(x = wt, y = mpg, fill = cyl)) + 
  geom_point(size = 10, shape = 23, col = mycolor)



