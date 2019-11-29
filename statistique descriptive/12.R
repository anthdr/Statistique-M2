library(tidyverse)

table2 <- table2
table4a <- table4a
table4b <- table4b
#table 2 is organized with country and year for every observation
#for type and count, type define the value observed and amount is the value observed
#table 4a has country and years for variable and the value is cases
#table 4b has country and years for variable and the value is population

table2pop <- filter(table2, type == "population")
table2cas <- filter(table2, type == "cases")
table2a <- merge(table2pop, table2cas, by = "country")
