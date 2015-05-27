library(dplyr)
library(tidyr)
library(xtable)


# Make a table
pugetSound
abbrev <- c("Edm - King", "Faunt - South", "Faunt - Vash", "Muk - Clint",
            "Pt Def - Tahleq", "Sea - Bain", "Sea - Brem", "South - Vash",
            "Key - Port Town")
mapping <- data.frame("route_name" = pugetSound, "routeShort" = abbrev)

puget <- notCrazy[notCrazy$route_name %in% pugetSound, ]
puget <- merge(puget, mapping)

abc <- table(puget$actual_vessel_name, puget$routeShort)
vessels <- rowSums(abc)
routes <- colSums(abc)
vorder <- sort(vessels, index.return = TRUE)$ix
horder <- sort(routes, index.return = TRUE)$ix
abc <- abc[vorder, horder]
abc <- cbind(abc, vessels[vorder])
abc <- rbind(abc, colSums(abc))

library(xtable)
xtable(abc, digits = 0)



# Make a table of delays by route

delays <- lapply(c(5, 10, 20, 30, 45, 60), function(i) {
  tmp <- aggregate(difference ~ route_name, data = puget, function(x) {
    mean(x > i, na.rm = TRUE)
  })
  names(tmp)[2] <- paste(">", i, "min", sep = "")
  tmp[2] <- tmp[2] * 100
  tmp[2] <- round(tmp[2], 1)
  tmp
})

delays <- Reduce(merge, delays)
meandelays <- aggregate(difference ~ route_name, data = notCrazy, FUN = mean)
names(meandelays)[2] <- "MeanDelay"
delays <- merge(delays, meandelays)
delays <- arrange(delays, -MeanDelay)

Totals <- unlist(lapply(c(5, 10, 20, 30, 45, 60), function(i) {
  round(100 * mean(puget$delay > i, na.rm = TRUE), 1)
}))
delays <- subset(delays, select = -c(MeanDelay))
delays <- rbind(delays, c("route_name" = "Average", Totals))

delays

xtable(delays, row.names = FALSE)





# Make a table of delays by vessel

delays <- lapply(c(5, 10, 20, 30, 45, 60), function(i) {
  tmp <- aggregate(difference ~ actual_vessel_name, data = puget, function(x) {
    mean(x > i, na.rm = TRUE)
  })
  names(tmp)[2] <- paste(">", i, "min", sep = "")
  tmp[2] <- tmp[2] * 100
  tmp[2] <- round(tmp[2], 1)
  tmp
})

delays <- Reduce(merge, delays)
meandelays <- aggregate(difference ~ actual_vessel_name, data = puget, FUN = mean)
names(meandelays)[2] <- "MeanDelay"
delays <- merge(delays, meandelays)
delays <- arrange(delays, -MeanDelay)

Totals <- unlist(lapply(c(5, 10, 20, 30, 45, 60), function(i) {
  round(100 * mean(puget$delay > i, na.rm = TRUE), 1)
}))
delays <- subset(delays, select = -c(MeanDelay))
delays <- rbind(delays, c("route_name" = "Average", Totals))

nCrossings <- aggregate(delay ~ actual_vessel_name, data = puget, FUN = length)
names(nCrossings) <- c("vessel", "n")

delays

xtable(delays, row.names = FALSE)



# Table by month of year...
puget$month <- lubridate::month(puget$departure_date, label = TRUE, abbr = TRUE)
puget$wday <- lubridate::wday(puget$departure_date, label = TRUE, abbr = TRUE)
#puget$wday[puget$departure_date %in% holidays] <- "Federal Holiday"

byMonth <- aggregate(difference ~ month * route_name, data = puget, FUN = mean)
byMonth <- spread(byMonth, month, difference)
byMonth <- cbind(byMonth, rowMeans(byMonth[, -1]))
names(byMonth)[14] <- "Ave"
byMonth <- plyr::arrange(byMonth, Ave)
byMonth <- rbind(byMonth, c(NA, colMeans(byMonth[, -1])))


xtable(byMonth, digits = 1)

xtable(aggregate(difference ~ month * route, data = puget, FUN = mean))


byDay <- aggregate(difference ~ wday * route_name, data = puget, FUN = mean)

byDay <- spread(byDay, wday, difference)
byDay <- cbind(byDay, rowMeans(byDay[, -1]))
names(byDay)[9] <- "Ave"
byDay <- plyr::arrange(byDay, Ave)
byDay <- rbind(byDay, c(NA, colMeans(byDay[, -1])))

xtable(byDay, digits = 1)

