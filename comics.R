set.seed(123)
setwd("E:/EDA/Exploring categorical data/")

library(dplyr)
library(bindrcpp)
library(ggplot2)

comics <- read.csv("comics.csv", header = T)

tab_cnt <- table(comics$id, comics$align)

# droplevels is used to drop a particular level.
comics <- comics %>% filter(align != "Reformed Criminals") %>% droplevels()

levels(comics$align)

ggplot(comics, aes(x = id, fill = align)) + geom_bar()
ggplot(comics, aes(x = id, fill = align)) + geom_bar() + facet_wrap(~align) + 
  scale_fill_manual(values = c("red","blue","yellow"))
 
ggplot(comics, aes(x = id, fill = align)) + geom_bar(position = "dodge")

ggplot(comics, aes(x = gender, fill = align)) + geom_bar(position = "dodge") +
  theme(axis.text.x = element_text(angle = 90))

addmargins(prop.table(tab_cnt))
addmargins(tab_cnt)

addmargins(prop.table(tab_cnt, margin = 1)) # Row
addmargins(prop.table(tab_cnt, margin = 2)) # Column

ggplot(comics, aes(x = id, fill = align)) + geom_bar(position = "fill") + 
  ylab("proportion")


ggplot(comics, aes(x = align, fill = gender)) + geom_bar(position = "fill") + 
  ylab("proportion")

ggplot(comics, aes(x = align, fill = gender)) + geom_bar()


ggplot(comics, aes(x = id)) + geom_bar() + facet_wrap(~align)
ggplot(comics, aes(x = id)) + geom_bar() + facet_wrap(~align+gender)

comics$align <- factor(comics$align, levels = c("Bad","Neutral","Good"))

levels(comics$align)

str(comics$align)

ggplot(comics, aes(x = align)) + geom_bar()

ggplot(comics, aes(x = align)) + geom_bar() + facet_wrap(~gender)


bp <- ggplot(comics, aes(x = "", fill = id)) + geom_bar(position = "fill") + xlab("ID")

bp + coord_polar("y", start = 0)


ggplot(comics, aes(x = "", fill = id)) + geom_bar(fill = "chartreuse", position = "fill") + coord_polar("y", start = 0)
ggplot(comics, aes(x = id)) + geom_bar(fill = "red") + theme(axis.text.x = element_text(angle = 90))

ggplot(comics %>% group_by(id) %>% summarise(count = n()), aes(x = "",y = count, fill = id)) + geom_bar(width = 1, stat = "identity") + 
  coord_polar("y", start = 0)


