library(readr)
setwd("C:/Users/hediera/Google Drive/Github/Statistique-M2/statistique descriptive")
e1clean <- read.table("e1clean.txt", header=TRUE, stringsAsFactors=FALSE)
summary(e1clean)
library(ggplot2)
ggplot(data=e1clean) +
  geom_bar(mapping = aes(x = condition, y = correct), stat  = "identity")
ggplot(data=e1clean) +
  geom_bar(mapping = aes(x = , y = correct), stat  = "identity")

ggplot(data=e1clean) +
  geom_bar(mapping = aes(x = condition, y = RT, fill = roi), stat = "summary", position = "dodge")
ggplot(data=e1clean) +
  geom_bar(mapping = aes(x = roi, y = RT, fill = condition), stat = "summary", position = "dodge")




library(dplyr)
e1clean <- filter(e1clean, e1clean$roi %in% c("RCNP", "RCV", "de", "headnoun", "Adv", "MainV", "MainO"))
roi_order <- c("RCNP", "RCV", "de", "headnoun", "Adv", "MainV", "MainO")
e1clean <- mutate(e1clean, roi=factor(roi, levels=roi_order))

ggplot(data=e1clean) +
  geom_point(mapping = aes(x = roi, y = RT, fill = condition, color = condition, shape = condition), stat = "summary", position = "dodge") +
  geom_line()
#les facteurs ont un ordrem pas les vecteurs


