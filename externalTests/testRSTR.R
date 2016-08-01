library(forecast)

#############################################

n = 50
trendSeasonalStructure = list(segments = list(c(0,1)), sKnots = list(c(1,0)))
ns = 5
seasonalStructure = list(segments = list(c(0,ns)), sKnots = c(as.list(1:(ns-1)),list(c(ns,0))))
seasons = rep(1:ns,n%/%ns+1)[1:n]
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

rstr1 = RSTR(data, predictors)

plot(rstr1)

#############################################

n = 50
trendSeasonalStructure = list(segments = list(c(0,1)), sKnots = list(c(1,0)))
ns = 5
seasonalStructure = list(segments = list(c(0,ns)), sKnots = c(as.list(1:(ns-1)),list(c(ns,0))))
seasons = rep(1:ns,n%/%ns+1)[1:n]
trendSeasons = rep(1, length(seasons))
times = seq_along(seasons)
dataOrig = seasons + times/4
data = dataOrig + rnorm(length(dataOrig), sd = 0.3)
plot(times, data, type = "l")
timeKnots = times
trendData = rep(1, n)
seasonData = rep(1, n)
trend = list(data = trendData, times = times, seasons = trendSeasons, timeKnots = timeKnots, seasonalStructure = trendSeasonalStructure, lambdas = c(1,0,0))
season = list(data = seasonData, times = times, seasons = seasons, timeKnots = timeKnots, seasonalStructure = seasonalStructure, lambdas = c(10,0,0))
predictors = list(trend, season)

rstr1 = RSTR(data, predictors, confidence = c(0.8, 0.95), nMCIter = 400)
rstr2 = AutoRSTR(data, predictors, confidence = c(0.8, 0.95), nMCIter = 400, gapCV = 1, reltol = 0.0001)

plot(rstr1)
plot(rstr2)

#############################################

mydata <- read.csv("Seasonal_data.csv")
ts1 = ts(mydata[,2], start = c(1982,4), frequency = 12)
ts2 = window(ts1, start = c(2000,1), end = c(2009,12))
plot(ts2)

trendSeasonalStructure = list(segments = list(c(0,1)), sKnots = list(c(1,0)))
seasonalStructure = list(segments = list(c(0,12)), sKnots = list(1,2,3,4,5,6,7,8,9,10,11,c(12,0)))
seasons = as.vector(cycle(ts2))
trendSeasons = rep(1, length(ts2))
times = as.vector(time(ts2))
data = as.vector(ts2)
timeKnots = times
trendData = rep(1, length(ts2))
seasonData = rep(1, length(ts2))
trend = list(data = trendData,
             times = times,
             seasons = trendSeasons,
             timeKnots = timeKnots,
             seasonalStructure = trendSeasonalStructure,
             lambdas = c(0.1,0,0))
season = list(data = seasonData,
              times = times,
              seasons = seasons,
              timeKnots = timeKnots,
              seasonalStructure = seasonalStructure,
              lambdas = c(10,0,0))
predictors = list(trend, season)

tm = system.time({str1 = STR(data, predictors)})
print(tm)
tm = system.time({str2 = RSTR(data, predictors)})
print(tm)

plot(str1)
plot(str2)

outl = rep(0,length(ts2))
outl[14] = 900
outl[113] = -700
tsOutl = ts(outl, start = c(2000,1), frequency = 12)
l = length(ts2)
uptr = rep(0,l)
uptr[60:l] = (60:l)-59
tsUptr = ts(uptr, start = c(2000,1), frequency = 12)
uts1 = log(ts2) - 0.01*tsUptr
uts2 = log(ts2 + tsOutl)
uts3 = log(ts2) - 0.3*(time(ts2) >= 2005)
plot(uts1)
plot(uts2)
plot(uts3)

ustr1 = AutoSTR(as.vector(uts1), predictors, confidence = 0.95, gapCV = 12, reltol = 0.001)
urstr1 = AutoRSTR(data = as.vector(uts1), predictors = predictors, confidence = 0.95, gapCV = 12, reltol = 0.001)
plot(ustr1)
plot(urstr1)

ustr2 = AutoSTR(as.vector(uts2), predictors, confidence = 0.95, gapCV = 12, reltol = 0.001)
urstr2 = AutoRSTR(as.vector(uts2), predictors, confidence = 0.95, gapCV = 12, reltol = 0.001)
plot(ustr2)
plot(urstr2)

ustr3 = AutoSTR(as.vector(uts3), predictors, confidence = 0.95, gapCV = 12, reltol = 0.001)
urstr3 = AutoRSTR(as.vector(uts3), predictors, confidence = 0.95, gapCV = 12, reltol = 0.001)
plot(ustr3)
plot(urstr3)

