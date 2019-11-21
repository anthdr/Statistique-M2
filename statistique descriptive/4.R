#4.4
#1 it works !
my_variable <- 10
my_variable

#2
library(tidyverse)

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy))
#wrong spelling
filter(mpg, cyl = 8)
#missing bracket
filter(diamond, carat > 3)
 
#3      
#alt shift k shows keyboard shortcuts