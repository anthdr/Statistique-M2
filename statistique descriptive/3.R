library(tidyverse)
library(ggplot2)
#create data
mpg <- ggplot2::mpg


#3.2.4
#2 234 observations (rows) and 11 variables (column)
?mpg
#3 drv is the type of traction wheels the vehicle uses
#4 
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy))
#5
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = class, y = drv))
#the plot does not involve numeric data


#3.3.1 Exercises
#1
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, color = "blue"))
#color = "blue" does not refer to a variable in mpg, it creates a variable (which is the same) for every row
#2 manufacturer, model, trans, drv, fl and class are categorical; displ, year, cty and hwy are continuous
#3
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, color = model, size = trans, shape = manufacturer))
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, color = displ, size = cty, alpha = hwy))
#Some aesthetics are more understandable for continuous data (alpha, size (gradient)), some for categorical data (shape, color).
#4
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, alpha = manufacturer, color = manufacturer))
#values are merged, they're overlapping
#5 stroke is used to modify the width of the border
#6 
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, colour = displ < 5)) +
  geom_point(shape = 21, colour = "black", fill = "white", size = 5, stroke = 5)
#if we don't use a variable name, the aesthetic will map itself to the x axis

#3.5.1
