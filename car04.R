set.seed(123)
setwd("E:/EDA/Datasets/")

car04 <- read.csv("cars04.csv", header = T, stringsAsFactors = F)

library(ggplot2)
library(dplyr)

dim(car04)

str(car04)

ggplot(car04, aes(x = city_mpg)) + geom_histogram(bins = 20) + facet_grid(~suv, labeller = "label_both")


gg_b <- ggplot_build(ggplot(car04, aes(x = city_mpg)) + geom_histogram(bins = 20) + facet_wrap(~suv, labeller = "label_both"))

bin_gg_b <- dim(gg_b$data[[1]])[1]

ggplot(car04, aes(x = city_mpg)) + geom_histogram(bins = 20, fill = rainbow(bin_gg_b)) + facet_wrap(~suv, labeller = "label_both")

unique(car04$ncyl)

common_cyl <- car04[car04$ncyl %in% c(4,6,8),]
unique(common_cyl$ncyl)

ggplot(common_cyl, aes(x = 1, y = city_mpg)) + geom_boxplot() + coord_flip() + facet_wrap(~ncyl)

ggplot(common_cyl, aes(x = (city_mpg))) + geom_density() + facet_wrap(~(ncyl))

common_cyl %>% 
  ggplot(aes(x = 1, y = city_mpg)) + 
  geom_boxplot() + 
  coord_flip() + 
  facet_wrap(~ncyl)

ggplot(car04, aes(x = as.factor(ncyl), y = city_mpg)) + geom_boxplot()

ggplot(car04, aes(x = city_mpg, fill = as.factor(ncyl))) + geom_density(alpha = 0.4)

#sapply(car04, unique)

car2 <- car04 %>% filter(eng_size > 2.0)

ggplot(car2, aes(x = hwy_mpg)) + geom_histogram()

car04 %>% 
  filter(eng_size < 2.0) %>%
  ggplot(aes(x = hwy_mpg)) + 
  geom_histogram()

car04 %>%
  filter(eng_size < 2.0) %>% 
  ggplot(aes(x = hwy_mpg)) + 
  geom_density(bw = 5)

car04 %>%
  filter(eng_size < 2.0) %>% 
  ggplot(aes(x = hwy_mpg)) + 
  geom_density()


car04 %>%
  ggplot(aes(horsepwr)) +
  geom_histogram(binwidth = 3) +
  ggtitle("Distribution of Horsepwr - Bin width (3)")

car04 %>%
  ggplot(aes(horsepwr)) +
  geom_histogram(binwidth = 30) +
  ggtitle("Distribution of Horsepwr - Bin width (30)")

car04 %>%
  ggplot(aes(horsepwr)) +
  geom_histogram(binwidth = 60) +
  ggtitle("Distribution of Horsepwr - Bin width (60)")

car_no <- car04 %>% filter(msrp < 100000)


ggplot(car04, aes(x = 1, y = msrp)) + geom_boxplot()
lk <- as.numeric(row.names(head(car04[order(-car04$msrp),],5)))
car.ex <- car04[-lk,]

ggplot(car.ex, aes(x = 1, y = msrp)) + geom_boxplot()


ggplot(car04, aes(x = city_mpg)) + geom_density()
ggplot(car04, aes(x = width)) + geom_density()

ggplot(car04, aes(x = msrp)) + 
  geom_density() + 
  facet_wrap(pickup+all_wheel ~ rear_wheel, labeller = label_both) + 
  ggtitle("Density plot on pickup and rear_wheel")

ggplot(car04, aes(x = msrp)) + 
  geom_density() + 
  facet_grid(pickup+all_wheel ~ rear_wheel, labeller = label_both) + 
  ggtitle("Density plot on pickup and rear_wheel")


table(car04$rear_wheel, car04$pickup)

unique(car04$hwy_mpg)

car04 %>% filter(ncyl %in% c(4,6,8)) %>% 
  ggplot(aes(x = hwy_mpg)) + geom_histogram() + facet_grid(ncyl~suv, labeller = "label_both") + 
  ggtitle("Histogram Faceted on ncyl and suv")
