library(dplyr)
library(ggplot2)
library(lubridate)

# Wait, Stednick defines his own infix functions? Whoa
`%notin%` <- function(x,y) !(x %in% y)

raw <- read.csv("data/PDR-15-1195.csv", colClasses="character")
raw$scheduled_departure_dt <- as.POSIXlt(raw$scheduled_departure)
raw$actual_departure_dt <- as.POSIXlt(raw$actual_departure)
raw$scheduled_hour <- raw$scheduled_departure_dt$hour
raw$difference <- as.numeric(difftime(raw$actual_departure, raw$scheduled_departure_dt, units = "min"))
raw$departure_date <- as.Date(raw$scheduled_departure)
# raw$scheduled_departure <- as.Date(raw$scheduled_departure)

# I'm going to put a federal holidays thing in here so that they 
# don't get confused with weekdays. There's also the WSDOT holidays
# which is a subset of this list, to be mindful of.
holidays <- as.Date(c("2014-01-01", "2014-01-20", "2014-02-17",
                      "2014-05-26", "2014-07-04", "2014-09-01",
                      "2014-10-13", "2014-11-11",
                      "2014-11-27", "2014-12-25"))

# Classify days
raw$wday <- raw$scheduled_departure_dt$wday
raw$day <- "Weekday"
raw$day[raw$wday == 0 | raw$wday == 6] <- "Weekend"
raw$day[raw$departure_date %in% holidays] <- "Holiday"

# Make a delay variable. We only care if it's positive?
# I think the big "negative" delays are data entry errors
raw[raw$difference < -100, ]
raw$delay <- abs(raw$difference)
raw$delay[raw$difference < 0] <- NA


# Make some brutal plots of all the raw data
all <- ggplot(raw[which(raw$delay >= 0), ]) + theme_bw() +
  geom_point(aes(departure_date, scheduled_hour, size = delay, col = day)) +
  facet_wrap(~departure_terminal) + 
  scale_size_continuous(range = c(.1, 6)) + 
  ggtitle("Delays for All Sailings by Departure Location")+
  scale_x_date(labels = scales::date_format("%b")) +
  xlab("Departure Date") + ylab("Scheduled Hour of Departure")
all
ggsave(file = "../all.png", all, width = 9, height = 7)

table(raw$route_name)
sanJuans <- c("Anacortes - Friday Harbor",
              "Anacortes - Lopez",
              "Anacortes - Orcas",
              "Anacortes - Shaw",
              "Lopez - Friday Harbor",
              "Lopez - Orcas",
              "Lopez - Shaw",
              "Orcas - Friday Harbor",
              "Orcas - Shaw",
              "Shaw - Friday Harbor")

all2 <- ggplot(raw[which(raw$delay >= 0 & raw$route_name %in% sanJuans), ]) + theme_bw() +
  geom_point(aes(departure_date, scheduled_hour, size = delay, col = day)) +
  facet_wrap(~route_name) + 
  scale_size_continuous(range = c(.1, 6)) + 
  ggtitle("Delays for All 2014 San Juan Island Sailings")+
  scale_x_date(labels = scales::date_format("%b")) +
  xlab("Departure Date") + ylab("Scheduled Hour of Departure")+ 
  scale_y_continuous(breaks = c(0, 5, 9, 13, 17, 20, 23), labels = c("Midnight", "5 am", "9 am", "1 pm", "5 pm", "8pm", "11pm"))
all2
ggsave(file = "../allSanJuan.png", all2, width = 9, height = 7)


pugetSound <- c("Fauntleroy - Southworth",
                "Fauntleroy - Vashon",
                "Mukilteo - Clinton",
                "Pt. Defiance - Tahlequah",
                "Seattle - Bainbridge Island",
                "Seattle - Bremerton",
                "Southworth - Vashon",
                "Keystone - Port Townsend")

all3 <- ggplot(raw[which(raw$delay >= 0 & raw$route_name %in% pugetSound), ]) + theme_bw() +
  geom_point(aes(departure_date, scheduled_hour, size = delay, col = day)) +
  facet_wrap(~route_name) + 
  scale_size_continuous(range = c(.1, 6)) + 
  ggtitle("Delays for All 2014 Puget Sound Sailings")+
  scale_x_date(labels = scales::date_format("%b")) +
  xlab("Departure Date") + ylab("Scheduled Hour of Departure") + 
  scale_y_continuous(breaks = c(0, 5, 9, 13, 17, 20, 23), labels = c("Midnight", "5 am", "9 am", "1 pm", "5 pm", "8pm", "11pm"))
all3
ggsave(file = "../allPugetSound.png", all3, width = 9, height = 7)


# Same view just for bainbridge
bainbridge <- ggplot(raw[which(raw$departure_terminal == "Bainbridge"), ]) + theme_bw() +
  geom_point(aes(departure_date, scheduled_hour, size = delay, col = day)) +
  scale_size_continuous(range = c(.5, 7), name = "Minutes Delayed") +
  ggtitle("Delays For All 2014 Sailings Originating from Bainbridge") +
  xlab("Date") + ylab("Hour of Day of scheduled departure") +
  scale_colour_discrete(name = "") + 
  scale_y_continuous(breaks = c(0, 5, 9, 13, 17, 20, 23), labels = c("Midnight", "5 am", "9 am", "1 pm", "5 pm", "8pm", "11pm"))
bainbridge
ggsave(file = "../bainbridge.png", bainbridge, width = 7, height = 5)


# Same view just for Port Townsend
pt <- ggplot(raw[which(raw$departure_terminal == "Port Townsend"), ]) + theme_bw() +
  geom_point(aes(departure_date, scheduled_hour, size = delay, col = day)) +
  scale_size_continuous(range = c(.5, 7), name = "Minutes Delayed") +
  ggtitle("Delays For All 2014 Sailings Originating from Port Townsend") +
  xlab("Date") + ylab("Hour of Day of scheduled departure") +
  scale_colour_discrete(name = "") + 
  scale_y_continuous(breaks = c(0, 5, 9, 13, 17, 20, 23), labels = c("Midnight", "5 am", "9 am", "1 pm", "5 pm", "8pm", "11pm"))
pt
ggsave(file = "../townsend.png", pt, width = 7, height = 5)




# Using just the non-crazy rows...
raw[abs(raw$difference) > 121, ]
notCrazy <- raw[abs(raw$difference) < 121, ]
hourly <- do.call('rbind', by(notCrazy, notCrazy$scheduled_hour, function(x) {
  tmp <- aggregate(difference ~ departure_terminal, data = x, FUN = mean)
  tmp$hour <- x$scheduled_hour[1]
  tmp
}))

# Summarize by week?
notCrazy$week <- week(notCrazy$actual_departure)
weekly <- do.call('rbind', by(notCrazy, interaction(notCrazy$week, notCrazy$departure_terminal), function(x) {
  data.frame("min" = min(x$difference), 
             "max" = max(x$difference), 
             "median" = median(x$difference),
             "week" = x$week[1],
             "departure_terminal" = x$departure_terminal[1])
}))
weekly <- do.call('rbind', by(weekly, weekly$departure_terminal, function(x) {
  missingWeeks <- setdiff(1:53, unique(x$week))
  if(length(missingWeeks)) {
    x <- rbind(x, 
                    data.frame("departure_terminal" = x$departure_terminal[1], 
                               "week" = missingWeeks,
                               "min" = NA,
                               "max" = NA,
                               "median" = NA))
  }
  x
}))

wlong <- reshape2::melt(weekly, id.vars = c("week", "departure_terminal"))
wplot <- ggplot(wlong) + theme_bw() + 
  geom_line(aes(x = week, y = value, linetype = variable)) + 
  facet_wrap(~departure_terminal) + 
  ggtitle("Difference from Scheduled to Actual Departure WSDOT Ferries 2014\nWeekly Min, Max, and Median - Boxed by Departure Terminal") +
  xlab("Week of Year") + ylab("Actual Departure Minutes from Scheduled Departure") + 
  scale_linetype_manual(name = "Weekly Summaries", values = c("dotted", "dashed", "solid")) #+
  #scale_colour_discrete(name = "Weekly Summaries")
wplot
ggsave(file = "../weeklyPlot.png", wplot, width = 9, height = 7)


# Get a median delay time by departure terminal & day type
abc <- do.call('rbind', by(notCrazy, interaction(notCrazy$day, notCrazy$departure_terminal), function(x) {
  data.frame("diff" = median(x$difference),
             "day" = x$day[1],
             "departure_terminal" = x$departure_terminal[1])
}))

medians <- do.call('rbind', by(notCrazy, notCrazy$departure_terminal, function(x) {
  data.frame("q05" = as.numeric(quantile(x$difference, .05)), 
             "q25"= as.numeric(quantile(x$difference, .25)),
             "q50" = median(x$difference),
             "q75" = as.numeric(quantile(x$difference, .75)), 
             "q95" = as.numeric(quantile(x$difference, .95)),
             "mean" = mean(x$difference), 
             "departure_terminal" = x$departure_terminal[1])
}))
medians <- arrange(medians, mean)
medians$departure_terminal <- factor(medians$departure_terminal, levels = medians$departure_terminal)
mlong <- reshape2::melt(medians, id.vars = "departure_terminal")
dPlot <- ggplot(mlong[mlong$variable != "mean", ]) + theme_bw() + 
  geom_point(aes(x = value, y = departure_terminal, shape = variable, col = variable)) +
  scale_colour_discrete(name = "Summary", labels = c("5% Quantile", "25% quantile", "median", "75% quantile", "95% quantile")) +
  scale_shape_discrete(name = "Summary", labels = c("5% Quantile", "25% quantile", "median", "75% quantile", "95% quantile")) +
  ggtitle("WSDOT Ferries 2014 Scheduled vs Actual Departure Time\nSorted by Average Delay Time") + 
  xlab("Minutes Difference between Scheduled and Actual Departure") +
  ylab("Departure Terminal")
dPlot
ggsave(file = "../quantiles.png", dPlot, width = 7, height = 5)



# Do it again by route
medians <- do.call('rbind', by(notCrazy, notCrazy$route_name, function(x) {
  data.frame("q05" = as.numeric(quantile(x$difference, .05)), 
             "q25"= as.numeric(quantile(x$difference, .25)),
             "q50" = median(x$difference),
             "q75" = as.numeric(quantile(x$difference, .75)), 
             "q95" = as.numeric(quantile(x$difference, .95)),
             "mean" = mean(x$difference), 
             "route" = x$route_name[1])
}))
medians <- arrange(medians, mean)
medians$route <- factor(medians$route, levels = medians$route)
mlong <- reshape2::melt(medians, id.vars = "route")
dPlot <- ggplot(mlong[mlong$variable != "mean", ]) + theme_bw() + 
  geom_point(aes(x = value, y = route, shape = variable, col = variable)) +
  scale_colour_discrete(name = "Summary", labels = c("5% Quantile", "25% quantile", "median", "75% quantile", "95% quantile")) +
  scale_shape_discrete(name = "Summary", labels = c("5% Quantile", "25% quantile", "median", "75% quantile", "95% quantile")) +
  ggtitle("WSDOT Ferries 2014 Scheduled vs Actual Departure Time\nSorted by Average Delay Time") + 
  xlab("Minutes Difference between Scheduled and Actual Departure") +
  ylab("Route")
dPlot
ggsave(file = "../quantilesRoute.png", dPlot, width = 7, height = 5)






# Try another weekly plot but by vessel
weekly <- do.call('rbind', by(notCrazy, interaction(notCrazy$week, notCrazy$actual_vessel_name), function(x) {
  data.frame("min" = min(x$difference), 
             "max" = max(x$difference), 
             "median" = median(x$difference),
             "week" = x$week[1],
             "vessel" = x$actual_vessel_name[1])
}))
weekly <- do.call('rbind', by(weekly, weekly$vessel, function(x) {
  missingWeeks <- setdiff(1:53, unique(x$week))
  if(length(missingWeeks)) {
    x <- rbind(x, 
               data.frame("vessel" = x$vessel[1], 
                          "week" = missingWeeks,
                          "min" = NA,
                          "max" = NA,
                          "median" = NA))
  }
  x
}))

wlong <- reshape2::melt(weekly, id.vars = c("week", "vessel"))
wplot <- ggplot(wlong) + theme_bw() + 
  geom_line(aes(x = week, y = value, linetype = variable)) + 
  facet_wrap(~vessel) + 
  ggtitle("Difference from Scheduled to Actual Departure WSDOT Ferries 2014\nWeekly Min, Max, and Median - Boxed by Departure Terminal") +
  xlab("Week of Year") + ylab("Actual Departure Minutes from Scheduled Departure") + 
  scale_linetype_manual(name = "Weekly Summaries", values = c("dotted", "dashed", "solid")) #+
#scale_colour_discrete(name = "Weekly Summaries")
wplot
ggsave(file = "../weeklyPlotVessel.png", wplot, width = 9, height = 7)




