

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


