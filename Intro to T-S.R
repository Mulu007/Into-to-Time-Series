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
