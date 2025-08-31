library(forecast)
library(stR)

mydata <- read.csv("../externalTests/Seasonal_data.csv")
ts1 = ts(
  BoxCox(mydata[, 2], BoxCox.lambda(mydata[, 2])),
  start = c(1982, 4),
  frequency = 12
)
ts2 = window(ts1, start = c(2000, 1), end = c(2009, 12))
times2 = time(ts2)

plot(ts1, ylab = "Box-Cox $ Millions", xlab = "", main = "")
plot(ts2, ylab = "Box-Cox $ Millions", xlab = "", main = "")

trendSeasonalStructure = list(segments = list(c(0, 1)), sKnots = list(c(1, 0)))
seasonalStructure = list(
  segments = list(c(0, 12)),
  sKnots = list(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, c(12, 0))
)
seasons = as.vector(cycle(ts2))
trendSeasons = rep(1, length(ts2))
times = as.vector(time(ts2))
data = as.vector(ts2)
timeKnots = times
trendData = rep(1, length(ts2))
seasonData = rep(1, length(ts2))
trend = list(
  data = trendData,
  times = times,
  seasons = trendSeasons,
  timeKnots = timeKnots,
  seasonalStructure = trendSeasonalStructure,
  lambdas = c(1, 0, 0),
  name = "Trend"
)
season = list(
  data = seasonData,
  times = times,
  seasons = seasons,
  timeKnots = timeKnots,
  seasonalStructure = seasonalStructure,
  lambdas = c(10, 0, 0),
  name = "Seasonal"
)
predictors = list(trend, season)

tm = system.time({
  str1 = STRmodel(data, predictors)
})
print(tm)

plot(str1)
comp = components(str1)
plot(comp)

fit <- AutoSTR(log(grocery))
comp = components(fit)
plot(comp)
plot(stR::seasadj(fit))
