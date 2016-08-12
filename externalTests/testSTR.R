library(forecast)
library(stR)

#############################################

n = 50
trendSeasonalStructure = list(segments = list(c(0,1)), sKnots = list(c(1,0)))
ns = 5
seasonalStructure = list(segments = list(c(0,ns)), sKnots = c(as.list(1:(ns-1)),list(c(ns,0))))
# seasons = rep(1:ns, n%/%ns+1)[1:n]
seasons = (0:(n-1))%%ns + 1
trendSeasons = rep(1, length(seasons))
times = seq_along(seasons)
data = seasons + times/4
plot(times, data, type = "l")
timeKnots = times
trendData = rep(1, n)
seasonData = rep(1, n)
trend = list(data = trendData, times = times, seasons = trendSeasons, timeKnots = timeKnots, seasonalStructure = trendSeasonalStructure, lambdas = c(1,0,0))
season = list(data = seasonData, times = times, seasons = seasons, timeKnots = timeKnots, seasonalStructure = seasonalStructure, lambdas = c(10,0,0))
predictors = list(trend, season)

str1 = STR(data, predictors)

# plot(str1$output$random$data, type = "l")
# plot(str1$output$predictors[[1]]$data, type = "l")
# plot(str1$output$predictors[[2]]$data, type = "l")

plot(str1)

oldData = data

data = oldData
data[c(3,4,7,20,24,29,35,37,45)] = NA
plot(times, data, type = "l")
str2 = STR(data, predictors)
plot(str2)

data = data + rnorm(length(data), 0, 0.2)
plot(times, data, type = "l")
str3 = STR(data, predictors)
plot(str3)

str4 = STR(data, predictors, confidence = 0.95)
plot(str4)

#############################################

n = 50
trendSeasonalStructure = list(segments = list(c(0,1)), sKnots = list(c(1,0)))
ns = 5
seasonalStructure = list(segments = list(c(0,ns)), sKnots = c(as.list(1:(ns-1)),list(c(ns,0))))
seasons = (0:(n-1))%%ns + 1
trendSeasons = rep(1, length(seasons))
times = seq_along(seasons)
data = seasons + times/4
set.seed(1234567890)
data = data + rnorm(length(data), 0, 0.4)
plot(times, data, type = "l")
timeKnots = times
trendData = rep(1, n)
seasonData = rep(1, n)
trend = list(data = trendData, times = times, seasons = trendSeasons, timeKnots = timeKnots, seasonalStructure = trendSeasonalStructure, lambdas = c(1,0,0))
season = list(data = seasonData, times = times, seasons = seasons, timeKnots = timeKnots, seasonalStructure = seasonalStructure, lambdas = c(1,1,1))
predictors = list(trend, season)
str = AutoSTR(data, predictors, reltol = 0.001, gapCV = 7, confidence = 0.95)
plot(str)

#############################################

n = 70
trendSeasonalStructure = list(segments = list(c(0,1)), sKnots = list(c(1,0)))
ns = 5
seasonalStructure = list(segments = list(c(0,ns)), sKnots = c(as.list(1:(ns-1)),list(c(ns,0))))
seasons = (0:(n-1))%%ns + 1
trendSeasons = rep(1, length(seasons))
times = seq_along(seasons)
data = seasons + times/4
set.seed(1234567890)
data = data + rnorm(length(data), 0, 0.2)
data[20] = data[20]+3
data[50] = data[50]-5
plot(times, data, type = "l")
timeKnots = times
trendData = rep(1, n)
seasonData = rep(1, n)
trend = list(data = trendData, times = times, seasons = trendSeasons, timeKnots = timeKnots, seasonalStructure = trendSeasonalStructure, lambdas = c(1,0,0))
season = list(data = seasonData, times = times, seasons = seasons, timeKnots = timeKnots, seasonalStructure = seasonalStructure, lambdas = c(1,0,1))
predictors = list(trend, season)
rstr = RSTR(data, predictors, confidence = 0.8)
plot(rstr)

rstr2 = AutoRSTR(data, predictors, reltol = 0.0000001, gapCV = 10, confidence = 0.95, nMCIter = 400)
plot(rstr2)

#############################################

mydata <- read.csv("../externalTests/Seasonal_data.csv")
ts1 = ts(BoxCox(mydata[,2], BoxCox.lambda(mydata[,2])), start = c(1982,4), frequency = 12)
ts2 = window(ts1, start = c(2000,1), end = c(2009,12))
times2 = time(ts2)

plot(ts1, ylab="Box-Cox $ Millions", xlab = "", main = "")
plot(ts2, ylab="Box-Cox $ Millions", xlab = "", main = "")

trendSeasonalStructure = list(segments = list(c(0,1)), sKnots = list(c(1,0)))
seasonalStructure = list(segments = list(c(0,12)), sKnots = list(1,2,3,4,5,6,7,8,9,10,11,c(12,0)))
seasonalStructure2 = list(segments = list(c(1,12)), sKnots = list(1,2,3,4,5,6,7,8,9,10,11,12))
seasons = as.vector(cycle(ts2))
trendSeasons = rep(1, length(ts2))
times = as.vector(time(ts2))
data = as.vector(ts2)
timeKnots = times
trendData = rep(1, length(ts2))
seasonData = rep(1, length(ts2))
trend = list(data = trendData, times = times, seasons = trendSeasons, timeKnots = timeKnots, seasonalStructure = trendSeasonalStructure, lambdas = c(1,0,0))
season = list(data = seasonData, times = times, seasons = seasons, timeKnots = timeKnots, seasonalStructure = seasonalStructure, lambdas = c(10,0,0))
season2 = list(data = seasonData, times = times, seasons = seasons, timeKnots = timeKnots, seasonalStructure = seasonalStructure2, lambdas = c(10,0,0))
predictors = list(trend, season)
predictors2 = list(trend, season2)

tm = system.time({str1 = STR(data, predictors)})
print(tm)
tm2 = system.time({
  str2 = STR(data, predictors2)
})
print(tm2)

plot(str1)
plot(str2)

###

trendSeasonalStructure = list(segments = list(c(0,1)), sKnots = list(c(1,0)))
seasonalStructure = list(segments = list(c(0,12)), sKnots = list(1,2,3,4,5,6,7,8,9,10,11,c(12,0)))
seasons = as.vector(cycle(ts1))
trendSeasons = rep(1, length(ts1))
times = as.vector(time(ts1))
data = as.vector(ts1)
trendTimeKnots = seq(from = stR:::first(times), to = stR:::last(times), length.out = 175)
# trendTimeKnots = times
seasonTimeKnots = seq(from = stR:::first(times), to = stR:::last(times), length.out = 15)
# seasonTimeKnots = times
trendData = rep(1, length(ts1))
seasonData = rep(1, length(ts1))
trend = list(name = "Trend", data = trendData, times = times, seasons = trendSeasons, timeKnots = trendTimeKnots, seasonalStructure = trendSeasonalStructure, lambdas = c(0.5,0,0))
season = list(name = "Yearly seasonality", data = seasonData, times = times, seasons = seasons, timeKnots = seasonTimeKnots, seasonalStructure = seasonalStructure, lambdas = c(40,0,0))
predictors = list(trend, season)

tm = system.time({str2 = STR(data, predictors)})
print(tm)

plot(str2)
plot(str2,
         dataScreens = c(1), predictorScreens = list(c(1,2),2), randomScreens = c(1,2,3),
         dataColor = "black", predictorColors = c("green", "blue"), randomColor = "red")
plot(str2,
         dataScreens = c(1), predictorScreens = list(1,c(1,2)), randomScreens = c(1,2,3),
         dataColor = "black", predictorColors = c("green", "blue"), randomColor = "red")

tm = system.time({sd3 = stR:::STRDesign(predictors)})
print(tm)

lambdas = list(list(lambdas = c(0.05,0,0)), list(lambdas = c(10,0,0)))
tm = system.time({str3 = STR(data, strDesign = sd3, lambdas = lambdas)})
print(tm)
plot(str3)
plot(str3,
         dataScreens = c(1), predictorScreens = list(c(1,2),c(1,3)), randomScreens = c(1,4),
         dataColor = "black", predictorColors = c("green", "blue"), randomColor = "red")


tm = system.time({ str4 = AutoSTR(data, predictors, confidence = 0.95) }); print(tm)
plot(str4,
         dataScreens = c(1), predictorScreens = list(c(1,2),c(1,3)), randomScreens = c(1,4),
         dataColor = "black", predictorColors = c("green", "blue"), randomColor = "red")

tm = system.time({str5 = STR(data, predictors, confidence = c(0.95))})
print(tm)
plot(str5)

n = 5
strDesign = stR:::STRDesign(predictors, norm = 2)

tm = system.time({ str4 = AutoSTR(data, predictors, confidence = 0.95) }); print(tm)
plot(str4,
         dataScreens = c(1), predictorScreens = list(c(1,2),c(1,3)), randomScreens = c(1,4),
         dataColor = "black", predictorColors = c("green", "blue"), randomColor = "red")

data[5] = NA
str5 = AutoSTR(data, predictors, confidence = 0.95)
plot(str5,
         dataScreens = c(1), predictorScreens = list(c(1,2),c(1,3)), randomScreens = c(1,4),
         dataColor = "black", predictorColors = c("green", "blue"), randomColor = "red")

str6 = AutoSTR(data, predictors, confidence = 0.95, gap = 5)
plot(str6,
         dataScreens = c(1), predictorScreens = list(c(1,2),c(1,3)), randomScreens = c(1,4),
         dataColor = "black", predictorColors = c("green", "blue"), randomColor = "red")

#############################################
