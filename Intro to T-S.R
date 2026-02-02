y <- ts(c(123, 39, 78, 52, 110), start = 2012, frequency = 1)

# fpp2 is a comprehensive library designed for series forecasting
# It automatically loads forecast, ggplot2, fma & expsmooth
install.packages("fpp2")
library(fpp2)

# plot-1
autoplot(melsyd[, "Economy.Class"]) +
  ggtitle("Economy class passengers: Melbourne-Sydney") +
  xlab("Year") +
  ylab("Thousands")

print(melsyd)

#plot-2
autoplot(a10) +
  ggtitle("Monthly sales of Antidiabetic drug in Australia") +
  ylab("$ million") +
  xlab("Year")

# Seasonal plots
# Similar to time plot but data is plotted against the individual season
# seasonal_plot_1
ggseasonplot(a10, year.labels = TRUE, year.labels.left = TRUE) +
  ylab("$ million") +
  ggtitle("Seasonal plot: antidiabetic drug sales")

# seasonal_plot_2
ggseasonplot(a10, polar=TRUE) +
  ylab("$ million") +
  ggtitle("Polar seasonal plot: antidiabetic drug sales")

# Seasonal subseries plots
# Seasonal patterns are collected together in separate mini time plots
# The horizontal lines means the mean for each month
ggsubseriesplot(a10) +
  ylab("$ million") +
  ggtitle("Seasonal subseries plot: antidiabetic drug sales")

# Scatterplots
print(elecdemand)
autoplot(elecdemand[,c("Demand","Temperature")], facets=TRUE) +
  xlab("Year: 2014") + ylab("") +
  ggtitle("Half-hourly electricity demand: Victoria, Australia")

as.data.frame(elecdemand) |>
  ggplot(aes(x = Temperature, y = Demand)) +
  geom_point() +
  ylab("Demand (GW)") + xlab("Temperature (Celcius)")
