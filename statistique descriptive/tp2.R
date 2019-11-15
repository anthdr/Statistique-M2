library(nycflights13)
library(tidyverse)

nycflights13 <- nycflights13::flights
jan1 <- filter(flights, month == 1, day == 1)

q1.1 <- filter(flights, arr_delay > 120)
q1.2 <- filter(flights, dest == "IAH" | dest == "HOU")
q1.2 <- filter(flights, dest %in% c("IAH", "HOU"))
?flights
view(airlines)
q1.3 <- filter(flights, carrier %in% c("UA", "DL", "AA"))
q1.4 <- filter(flights, month %in% c(7, 8, 9))
q1.5 <- filter(flights, arr_delay > 120 & dep_delay < 1)
q1.6 <- filter(flights, arr_delay > 60 & dep_delay < 30)
q1.7 <- filter(flights, hour %in% c(0, 1, 2, 3, 4, 5, 6))

q2.4 <- between(flights, flights$month == 7, flights$month == 9)
q2.4 <- filter(flights, between(flights$month, 7, 9))
q2.7 <- filter(flights, between(flights$hour, 0, 6))

