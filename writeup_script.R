suppressPackageStartupMessages(library(dplyr))
library(ggplot2)

`%notin%` <- function(x,y) !(x %in% y)

raw <- read.csv("data/PDR-15-1195.csv", colClasses="character")
raw$scheduled_departure_dt <- as.POSIXlt(raw$scheduled_departure)
raw$scheduled_departure <- as.Date(raw$scheduled_departure)
raw$actual_departure_dt <- as.POSIXlt(raw$actual_departure)
raw$scheduled_hour <- raw$scheduled_departure_dt$hour
raw$difference <- as.numeric(difftime(raw$actual_departure, raw$scheduled_departure_dt, units = "min"))
raw$departure_date <- as.Date(raw$scheduled_departure)

#classify days in raw data set
raw$wday <- raw$scheduled_departure_dt$wday
raw$day <- "Weekday"
raw$day[raw$wday == 0 | raw$wday == 6] <- "Weekend"

holidays <- as.Date(c("2014-01-01", "2014-01-20", "2014-02-17",
                      "2014-05-26", "2014-07-04", "2014-09-01",
                      "2014-10-13", "2014-11-11",
                      "2014-11-27", "2014-12-25"))
raw$day[raw$departure_date %in% holidays] <- "Holiday"

raw$delay <- abs(raw$difference)
raw$delay[raw$difference < 0] <- NA

pugetSound <- c("Edmonds - Kingston",
                "Fauntleroy - Southworth",
                "Fauntleroy - Vashon",
                "Mukilteo - Clinton",
                "Pt. Defiance - Tahlequah",
                "Seattle - Bainbridge Island",
                "Seattle - Bremerton",
                "Southworth - Vashon",
                "Keystone - Port Townsend")

all.days <-
  raw %>%
  subset(route_name %in% pugetSound) %>%
  select(scheduled_departure, route_name, difference) %>%
  group_by(scheduled_departure, route_name) %>%
  summarise_each(funs(mean), difference)

ggplot(all.days, aes(scheduled_departure, difference)) +
  geom_point() + stat_smooth(lwd=2) + xlab("Scheduled Departure Date") +
  ylab("Difference in Scheduled vs. Actual Departure Time \n in Minutes") +
  ggtitle("Mean Departure Delay for All Ferries in 2014")
ggsave("Figure1.png")

ggplot(all.days, aes(scheduled_departure, difference)) + geom_point() + stat_smooth() +
  scale_x_date(labels = scales::date_format("%b")) +
  facet_wrap(~route_name, scales="free_x") + xlab("Scheduled Departure Date") +
  ylab("Difference in Scheduled vs. Actual Departure Time \n in Minutes") +
  ggtitle("Mean Departure Delay for All Ferries in 2014")
ggsave("Figure2.png")


pugetSoundRoutes <-
  raw %>%
  subset(delay >= 0 & route_name %in% pugetSound)

hour.levels <- rev(0:23)
pugetSoundRoutes$scheduled_hour <- factor(pugetSoundRoutes$scheduled_hour, levels = 23:0, ordered=T)
names(pugetSoundRoutes)[14:15] <- c("Day", "Delay")

ggplot(pugetSoundRoutes) + 
  geom_point(aes(departure_date, scheduled_hour, size = Delay, col = Day)) +
  facet_wrap(~route_name, scales="free_x") + 
  scale_size_continuous(range = c(.1, 6)) + 
  ggtitle("Delays for All 2014 Puget Sound Sailings")+
  scale_x_date(labels = scales::date_format("%b")) +
  xlab("Departure Date") + ylab("Scheduled Hour of Departure") +
  scale_y_discrete(breaks = c(0, 5, 9, 13, 17, 20, 23), labels = c("Midnight", "5 am", "9 am", "1 pm", "5 pm", "8pm", "11pm"))
ggsave("Figure3.png")

fvs <- subset(pugetSoundRoutes, route_name %in% c("Fauntleroy - Southworth",
                "Fauntleroy - Vashon", "Southworth - Vashon"))
fvs$route_name <- "Fauntleroy - Southworth - Vashon"

ggplot(fvs) +
  geom_point(aes(departure_date, scheduled_hour, size = Delay, col = Day)) +
  facet_wrap(~route_name) +
  scale_size_continuous(range = c(.1, 6)) +
  ggtitle("Delays for All 2014 Sailings")+
  scale_x_date(labels = scales::date_format("%b")) +
  xlab("Departure Date") + ylab("Scheduled Hour of Departure") +
  scale_y_discrete(breaks = c(0, 5, 9, 13, 17, 20, 23), labels = c("Midnight", "5 am", "9 am", "1 pm", "5 pm", "8pm", "11pm"))
ggsave("Figure4.png")

bi.only <- subset(pugetSoundRoutes, route_name == "Seattle - Bainbridge Island")
ggplot(bi.only) + 
  geom_point(aes(departure_date, scheduled_hour, size = Delay, col = Day)) +
  facet_wrap(~route_name) + 
  scale_size_continuous(range = c(.1, 6)) + 
  ggtitle("Delays for All 2014 Sailings")+
  scale_x_date(labels = scales::date_format("%b")) +
  xlab("Departure Date") + ylab("Scheduled Hour of Departure") +
  scale_y_discrete(breaks = c(0, 5, 9, 13, 17, 20, 23), labels = c("Midnight", "5 am", "9 am", "1 pm", "5 pm", "8pm", "11pm"))
ggsave("Figure5.png")

raw <-
  raw %>%
  subset(Actual_Arrival != "NULL")
raw$actual_arrival <- as.Date(raw$Actual_Arrival)
raw$actual_arrival_dt <- as.POSIXlt(raw$Actual_Arrival)
raw$crossing <- as.numeric(difftime(raw$actual_arrival_dt, raw$actual_departure_dt, units = "min"))

mean.times <-
  raw %>%
  subset(route_name %in% pugetSound) %>%
  select(scheduled_departure, route_name, crossing) %>%
  group_by(scheduled_departure, route_name) %>%
  summarise_each(funs(mean), crossing)

routemean.crossing.times <-
  mean.times %>%
  group_by(route_name) %>%
  do(data.frame(routemean=mean(.$crossing)))

mean.times <- merge(mean.times, routemean.crossing.times)
mean.times$percentage <- mean.times$crossing / mean.times$routemean
ggplot(mean.times, aes(scheduled_departure, percentage)) + geom_point(size=0.3) + stat_smooth() +
  scale_x_date(labels = scales::date_format("%b")) +
  facet_wrap(~route_name, scales="free_x") + xlab("Scheduled Departure Date") +
  ylab("Percentage") +
  ggtitle("Average Crossing Time in Minutes as a Percentage of \n Mean 2014 Crossing Time")
ggsave("Figure6.png")
