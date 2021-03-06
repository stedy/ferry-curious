library(dplyr)
library(ggplot2)

`%notin%` <- function(x,y) !(x %in% y)

raw <- read.csv("data/PDR-15-1195.csv", colClasses="character")
raw$scheduled_departure_dt <- as.POSIXlt(raw$scheduled_departure)
raw$actual_departure <- as.POSIXlt(raw$actual_departure)
raw$difference <- abs(as.numeric(raw$actual_departure - raw$scheduled_departure_dt) / 60)
raw$scheduled_departure <- as.Date(raw$scheduled_departure)

#raw <- subset(raw, difference >= 0 & difference <= 5000)
ggplot2::qplot(raw$difference)

holidays <- as.Date(c("2014-01-01", "2014-05-26",
                      "2014-07-04", "2014-09-01",
                      "2014-11-27", "2014-11-25"))
regular.days <-
  raw %>%
  subset(scheduled_departure %notin% holidays) %>%
  select(scheduled_departure, route_name, difference) %>%
  group_by(scheduled_departure, route_name) %>%
  summarise_each(funs(mean), difference)

ggplot(regular.days, aes(scheduled_departure, difference, group=route_name, color=route_name)) +
  geom_path(alpha = 0.9)

seattle.bi <- subset(regular.days, route_name == "Seattle - Bainbridge Island")
seattle.bi$weekday <- weekdays(seattle.bi$scheduled_departure)

ggplot(seattle.bi, aes(scheduled_departure, difference, group=weekday, color=weekday)) +
  geom_path(alpha = 0.9)

#write as javascript date for mapping
seattle.bi$jdate <- str((seattle.bi$scheduled_departure - as.Date("1970-01-01")) * 60 * 60 * 24)
write.table(seattle.bi[c('jdate', 'difference')],  "afile.JSON", row.names=F,
            sep=":", quote=T)
