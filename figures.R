
notCrazy <- raw[abs(raw$difference) < 121 & raw$difference > -55, ]
puget <- notCrazy[notCrazy$route_name %in% pugetSound, ]

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
ggsave(file = "quantilesRoute.png", dPlot, width = 7, height = 5)


dPlot2 <- ggplot(mlong[mlong$variable != "mean" & mlong$route %in% pugetSound, ]) + theme_bw() + 
  geom_point(aes(x = value, y = route, shape = variable, col = variable)) +
  scale_colour_discrete(name = "Summary", labels = c("5% Quantile", "25% quantile", "median", "75% quantile", "95% quantile")) +
  scale_shape_discrete(name = "Summary", labels = c("5% Quantile", "25% quantile", "median", "75% quantile", "95% quantile")) +
  ggtitle("WSDOT Ferries 2014 Scheduled vs Actual Departure Time\nSorted by Average Delay Time") + 
  xlab("Minutes Difference between Scheduled and Actual Departure") +
  ylab("Route")
dPlot2
ggsave(file = "quantilesRoute2.pdf", dPlot2, width = 7, height = 5)


# Do it again by vessel
medians <- do.call('rbind', by(notCrazy, notCrazy$actual_vessel_name, function(x) {
  x <- x[x$route_name %in% pugetSound, ]
  if(!nrow(x)) return(NULL)
  data.frame("q05" = as.numeric(quantile(x$difference, .05)), 
             "q25"= as.numeric(quantile(x$difference, .25)),
             "q50" = median(x$difference),
             "q75" = as.numeric(quantile(x$difference, .75)), 
             "q95" = as.numeric(quantile(x$difference, .95)),
             "mean" = mean(x$difference), 
             "vessel" = x$actual_vessel_name[1])
}))
medians <- arrange(medians, mean)
medians$vessel <- factor(medians$vessel, levels = medians$vessel)
mlong <- reshape2::melt(medians, id.vars = "vessel")
dPlot <- ggplot(mlong[mlong$variable != "mean", ]) + theme_bw() + 
  geom_point(aes(x = value, y = vessel, shape = variable, col = variable)) +
  scale_colour_discrete(name = "Summary", labels = c("5% Quantile", "25% quantile", "median", "75% quantile", "95% quantile")) +
  scale_shape_discrete(name = "Summary", labels = c("5% Quantile", "25% quantile", "median", "75% quantile", "95% quantile")) +
  ggtitle("WSDOT Ferries 2014 Scheduled vs Actual Departure Time\nSorted by Average Delay Time") + 
  xlab("Minutes Difference between Scheduled and Actual Departure") +
  ylab("Vessel")
dPlot
ggsave(file = "quantilesVessel.pdf", dPlot, width = 7, height = 5)










library(dplyr)


all.days <-
  notCrazy %>%
  subset(route_name %in% pugetSound) %>%
  select(scheduled_departure, route_name, difference) %>%
  group_by(scheduled_departure) %>%
  summarise_each(funs(mean), difference)

delayPlot <- ggplot(all.days, aes(scheduled_departure, difference)) + theme_bw() +
  geom_point() + stat_smooth(lwd=2, se = FALSE) + xlab("Scheduled departure date") +
  ylab("Difference in scheduled vs. actual departure time \n in minutes") + 
  ggtitle("Average Daily Departure Delays - Puget Sound Routes in 2014")
delayPlot
ggsave(file = "annualdelays.pdf", delayPlot, width = 7, height = 5)




# Need to do something w/ time of day

byHour <- aggregate(difference ~ scheduled_hour * route_name, data = puget, FUN = mean)
nbyHour <- aggregate(difference ~ scheduled_hour * route_name, data = puget, FUN = length)
nbyHour <- nbyHour[nbyHour$difference > 100, ]
nbyHour$routehour <- interaction(nbyHour$route_name, nbyHour$scheduled_hour)
byHour$routehour <- interaction(byHour$route_name, byHour$scheduled_hour)
byHour <- byHour[byHour$routehour %in% nbyHour$routehour, ]


head(puget)
ggplot(byHour)  + theme_bw() + 
  geom_line(aes(x = scheduled_hour, y = difference, col = route_name))


puget$time  <- NA
puget$hour <- puget$scheduled_hour
puget$time[puget$hour >=4 & puget$hour <=10] <- "Morning"
puget$time[puget$hour >10 & puget$hour <=16] <- "Daytime"
puget$time[puget$hour >16 & puget$hour <=22] <- "Evening"
puget$time[puget$hour >22 | puget$hour <4] <- "Nighttime"
puget$time <- factor(puget$time, levels = c("Morning", "Daytime", "Evening", "Nighttime"))


byHour <- aggregate(difference ~ time * route_name, data = puget, FUN = mean)
nbyHour <- aggregate(difference ~ time * route_name, data = puget, FUN = length)
nbyHour <- nbyHour[nbyHour$difference > 100, ]
nbyHour$routehour <- interaction(nbyHour$route_name, nbyHour$time)
byHour$routehour <- interaction(byHour$route_name, byHour$time)
byHour <- byHour[byHour$routehour %in% nbyHour$routehour, ]

aves <- aggregate(difference ~ route_name, data = puget, FUN = mean)
aves <- arrange(aves, difference)
byHour$route_name <- factor(byHour$route_name, levels = aves$route_name)

timeofday <- ggplot(byHour)  + theme_bw() + 
  geom_bar(aes(x = time, y = difference, fill = route_name), stat = "identity", position = "dodge") +
  xlab("") + ylab("Average Difference Between Scheduled & Actual Departure \n(minutes)") +
  ggtitle("Departure Times by Time of Day and Route") +
  scale_fill_discrete(name = "Route")
timeofday
ggsave(file = "timeofday.pdf", timeofday, width = 7, height = 5)
