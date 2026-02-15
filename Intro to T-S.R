y <- ts(c(123, 39, 78, 52, 110), start = 2012, frequency = 1)

# fpp2 is a comprehensive library designed for series forecasting
# It automatically loads forecast, ggplot2, fma & expsmooth
install.packages("fpp2")
library(fpp2)
library(tidyverse)

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

# Scatterplot matrices
autoplot(visnights[,1:5], facets = TRUE) +
  ylab("Number of visitor nights each quarter (millions)")

install.packages("GGally")
GGally::ggpairs(as.data.frame(visnights[, 1:5]))

print(visnights)

# Lag plots
beer2 <- window(ausbeer, start=1992)
gglagplot(beer2)

# Autocorrelation
ggAcf(beer2)
print(Acf(beer2, plot = FALSE))

# Trends and seasonality in autocorrelation
aelec <- window(elec, start=1980)
autoplot(aelec) + xlab("Year") + ylab("GWh")
ggAcf(aelec, lag=48)

#white noise
set.seed(30)
y <- ts(rnorm(50))
autoplot(y) + ggtitle("White noise")
ggAcf(y)

# Exercise
# Daily morning gold prices in US dollars. 
# 1 January 1985 – 31 March 1989.
view(gold)
# Quarterly production of woollen yarn in Australia
# tonnes. Mar 1965 – Sep 1994.
view(woolyrn)
# Australian monthly gas production: 1956–1995.
view(gas)

autoplot(gold)
which.max(gold)
autoplot(woolyrnq)
autoplot(gas)

tute1 <- read.csv("tute1.csv", header = TRUE)
View(tute1)

# Converting the data to time series
# dataframe indexing
# data[row, column]
# tute1[,-1]
# Give me all rows but drop the first column
mytimeseries <- ts(tute1[,-1], start=1981, frequency=4)
view(mytimeseries)

# plotting
autoplot(mytimeseries, facets = TRUE)

# The second argument (skip=1) is required because the Excel sheet has two header rows.
retaildata <- readxl::read_excel("retail.xlsx", skip = 1)
myts <- ts(retaildata[, "A3349873A"],
           frequency = 12,
           start = c(1982, 4))
view(myts)

autoplot(myts)
ggseasonplot(myts, polar = TRUE)
ggsubseriesplot(myts)
gglagplot(myts)
ggAcf(myts)

# No. 4
# Annual bituminous coal production in the USA: 1920–1968.
view(bicoal)
ts_coal <- ts(bicoal, start = 1920)
autoplot(ts_coal)
ggseasonplot(ts_coal)
ggsubseriesplot(ts_coal)
gglagplot(ts_coal)
ggAcf(ts_coal)

###### FORECASTING METHODS ########################
beer2 <- window(ausbeer, start = 1992, end = c(2007,4))
view(beer2)

autoplot(beer2) +
  autolayer(meanf(beer2, h = 11),
            series = "Mean", PI = FALSE) +
  autolayer(naive(beer2, h = 11),
            series = "Naive", PI = FALSE) +
  autolayer(snaive(beer2, h = 11),
            series = "Seasonal naive", PI = FALSE) +
  ggtitle("Forecasts for quarterly beer production") +
  xlab("Year") + ylab("Megalitres") +
  guides(colour = guide_legend(title = "Forecast"))

# non seasonal methods on a 200 daily google stock chart
autoplot(goog200) +
  autolayer(meanf(goog200, h = 40),
            series = "Mean", PI = FALSE) +
  autolayer(rwf(goog200, h = 40),
            series = "Naive", PI = FALSE) +
  autolayer(rwf(goog200, drift = TRUE, h = 40),
            series = "Drift", PI = FALSE) +
  ggtitle("Google stock (daily ending 6 Dec 2013)") +
  xlab("Day") + ylab("Closing Price (US$)") +
  guides(colour = guide_legend(title = "Forecast"))

# Transformations and adjustments
# Calendar adjustments
dframe <- cbind(Monthly = milk,
                DailyAverage = milk/monthdays(milk))
autoplot(dframe, facet=TRUE) +
  xlab("Years") + ylab("Pounds") +
  ggtitle("Milk production per cow")

# Box- Cox Tranformations
(lambda <- BoxCox.lambda(elec))
autoplot(BoxCox(elec,lambda))

#Bias Adjustment
fc <- rwf(eggs, drift=TRUE, lambda=0, h=50, level=80)
fc2 <- rwf(eggs, drift=TRUE, lambda=0, h=50, level=80,
           biasadj=TRUE)
autoplot(eggs) +
  autolayer(fc, series="Simple back transformation") +
  autolayer(fc2, series="Bias adjusted", PI=FALSE) +
  guides(colour=guide_legend(title="Forecast"))
