library(forecast)
library(stR)

mydata <- read.csv("../externalTests/Seasonal_data.csv")
ts1 = ts(
  BoxCox(mydata[, 2], BoxCox.lambda(mydata[, 2])),
  start = c(1982, 4),
  frequency = 12
)
ts2 = window(ts1, start = c(2000, 1), end = c(2002, 12))
ts3 = window(ts1, start = c(1993, 1), end = c(2002, 12))
times2 = time(ts2)

plot(ts1, ylab = "Box-Cox $ Millions", xlab = "", main = "")
plot(ts2, ylab = "Box-Cox $ Millions", xlab = "", main = "")

################################################################

trendSeasonalStructure = list(segments = list(c(0, 1)), sKnots = list(c(1, 0)))
seasonalStructure = list(
  segments = list(c(0, 12)),
  sKnots = list(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, c(12, 0))
)
seasons = as.vector(cycle(ts2))
trendSeasons = rep(1, length(ts2))
times = as.vector(time(ts2))
data = as.vector(ts2)
data[tail(seq_along(data), 5)] = NA
timeKnots = times
trendData = rep(1, length(ts2))
seasonData = rep(1, length(ts2))
trend = list(
  data = trendData,
  times = times,
  seasons = trendSeasons,
  timeKnots = timeKnots,
  seasonalStructure = trendSeasonalStructure,
  lambdas = c(1, 0, 0)
)
season = list(
  data = seasonData,
  times = times,
  seasons = seasons,
  timeKnots = timeKnots,
  seasonalStructure = seasonalStructure,
  lambdas = c(10, 0, 0)
)
predictors = list(trend, season)

tm = system.time({
  str1 = STRmodel(data, predictors)
})
print(tm)
tm = system.time({
  str2 = RSTRmodel(data, predictors)
})
print(tm)

plot(str1)
plot(str2)

################################################################

n = 20
trendSeasonalStructure = list(segments = list(c(0, 1)), sKnots = list(c(1, 0)))
ns = 2
seasonalStructure = list(
  segments = list(c(0, ns)),
  sKnots = c(as.list(1:(ns - 1)), list(c(ns, 0)))
)
seasons = rep(1:ns, n %/% ns + 1)[1:n]
trendSeasons = rep(1, length(seasons))
times = seq_along(seasons)
data = seasons + times / 4
data[tail(seq_along(data), 15)] = NA
plot(times, data, type = "l")
timeKnots = times
trendData = rep(1, n)
seasonData = rep(1, n)
trend = list(
  data = trendData,
  times = times,
  seasons = trendSeasons,
  timeKnots = timeKnots,
  seasonalStructure = trendSeasonalStructure,
  lambdas = c(1, 0, 0)
)
season = list(
  data = seasonData,
  times = times,
  seasons = seasons,
  timeKnots = timeKnots,
  seasonalStructure = seasonalStructure,
  lambdas = c(10, 0, 0)
)
predictors = list(trend, season)

str1 = STRmodel(data, predictors)

plot(str1)

################################################################

n = 16
trendSeasonalStructure = list(segments = list(c(0, 1)), sKnots = list(c(1, 0)))
ns = 4
seasonalStructure = list(
  segments = list(c(0, ns)),
  sKnots = c(as.list(1:(ns - 1)), list(c(ns, 0)))
)
seasons = rep(1:ns, n %/% ns + 1)[1:n]
trendSeasons = rep(1, length(seasons))
times = seq_along(seasons)
data = seasons + times / 4
plot(times, data, type = "l")
timeKnots = times
trendData = rep(1, n)
seasonData = rep(1, n)
trend = list(
  data = trendData,
  times = times,
  seasons = trendSeasons,
  timeKnots = timeKnots,
  seasonalStructure = trendSeasonalStructure,
  lambdas = c(1, 0, 0)
)
season = list(
  data = seasonData,
  times = times,
  seasons = seasons,
  timeKnots = timeKnots,
  seasonalStructure = seasonalStructure,
  lambdas = c(10, 0, 10)
)
predictors = list(trend, season)

data[c(3, 4, 7, 16)] = NA
data = data + rnorm(length(data), 0, 0.1)

str1 = STRmodel(data, predictors)
plot(str1)

str2 = STRmodel(data, predictors, confidence = 0.95)
plot(str2)

################################################################

n = 6
trendSeasonalStructure = list(segments = list(c(0, 1)), sKnots = list(c(1, 0)))
ns = 2
seasonalStructure = list(
  segments = list(c(0, ns)),
  sKnots = c(as.list(1:(ns - 1)), list(c(ns, 0)))
)
seasons = rep(1:ns, n %/% ns + 1)[1:n]
trendSeasons = rep(1, length(seasons))
times = seq_along(seasons)
data = seasons + times / 4
plot(times, data, type = "l")
trendTimeKnots = c(min(times), max(times))
seasonTimeKnots = times
trendData = rep(1, n)
seasonData = rep(1, n)
trend = list(
  data = trendData,
  times = times,
  seasons = trendSeasons,
  timeKnots = trendTimeKnots,
  seasonalStructure = trendSeasonalStructure,
  lambdas = c(0, 0, 0)
)
season = list(
  data = seasonData,
  times = times,
  seasons = seasons,
  timeKnots = seasonTimeKnots,
  seasonalStructure = seasonalStructure,
  lambdas = c(10, 0, 0)
)
predictors = list(trend, season)

str1 = STRmodel(data, predictors)

plot(str1)

################################################################

n = 8
trendSeasonalStructure = list(segments = list(c(0, 1)), sKnots = list(c(1, 0)))
ns = 2
seasonalStructure = list(
  segments = list(c(0, ns)),
  sKnots = c(as.list(1:(ns - 1)), list(c(ns, 0)))
)
seasons = rep(1:ns, n %/% ns + 1)[1:n]
trendSeasons = rep(1, length(seasons))
times = seq_along(seasons)
data = seasons + times / 4
plot(times, data, type = "l")
trendTimeKnots = times
seasonTimeKnots = c(min(times), max(times))
trendData = rep(1, n)
seasonData = rep(1, n)
trend = list(
  data = trendData,
  times = times,
  seasons = trendSeasons,
  timeKnots = trendTimeKnots,
  seasonalStructure = trendSeasonalStructure,
  lambdas = c(10, 0, 0)
)
season = list(
  data = seasonData,
  times = times,
  seasons = seasons,
  timeKnots = seasonTimeKnots,
  seasonalStructure = seasonalStructure,
  lambdas = c(0, 0, 0)
)
predictors = list(trend, season)

str1 = STRmodel(data, predictors)

plot(str1)

################################################################

n = 8
trendSeasonalStructure = list(segments = list(c(0, 1)), sKnots = list(c(1, 0)))
ns = 2
seasonalStructure = list(
  segments = list(c(0, ns)),
  sKnots = c(as.list(1:(ns - 1)), list(c(ns, 0)))
)
seasons = rep(1:ns, n %/% ns + 1)[1:n]
trendSeasons = rep(1, length(seasons))
times = seq_along(seasons)
data = seasons + times / 4
plot(times, data, type = "l")
trendTimeKnots = c(min(times), max(times))
seasonTimeKnots = c(min(times), max(times))
trendData = rep(1, n)
seasonData = rep(1, n)
trend = list(
  data = trendData,
  times = times,
  seasons = trendSeasons,
  timeKnots = trendTimeKnots,
  seasonalStructure = trendSeasonalStructure,
  lambdas = c(0, 0, 0)
)
season = list(
  data = seasonData,
  times = times,
  seasons = seasons,
  timeKnots = seasonTimeKnots,
  seasonalStructure = seasonalStructure,
  lambdas = c(0, 0, 10)
)
predictors = list(trend, season)

str1 = STRmodel(data, predictors)

plot(str1)

################################################################

trendSeasonalStructure = list(segments = list(c(0, 1)), sKnots = list(c(1, 0)))
seasonalStructure = list(
  segments = list(c(0, 12)),
  sKnots = list(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, c(12, 0))
)
seasons = as.vector(cycle(ts2))
trendSeasons = rep(1, length(ts2))
times = as.vector(time(ts2))
data = as.vector(ts2)
data[tail(seq_along(data), 5)] = NA
timeKnots = times
trendData = rep(1, length(ts2))
seasonData = rep(1, length(ts2))
trend = list(
  data = trendData,
  times = times,
  seasons = trendSeasons,
  timeKnots = timeKnots,
  seasonalStructure = trendSeasonalStructure,
  lambdas = c(1, 0, 0)
)
season = list(
  data = seasonData,
  times = times,
  seasons = seasons,
  timeKnots = timeKnots,
  seasonalStructure = seasonalStructure,
  lambdas = c(10, 0, 0)
)
predictors = list(trend, season)

tm = system.time({
  str1 = STRmodel(data, predictors, confidence = c(0.8, 0.95))
})
print(tm)

plot(str1)

################################################################

trendSeasonalStructure = list(segments = list(c(0, 1)), sKnots = list(c(1, 0)))
seasonalStructure = list(
  segments = list(c(0, 12)),
  sKnots = list(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, c(12, 0))
)
seasons = as.vector(cycle(ts1))
trendSeasons = rep(1, length(ts1))
times = as.vector(time(ts1))
data = as.vector(ts1)
trendTimeKnots = seq(
  from = stR:::first(times),
  to = stR:::last(times),
  length.out = 175
)
seasonTimeKnots = seq(
  from = stR:::first(times),
  to = stR:::last(times),
  length.out = 15
)
trendData = rep(1, length(ts1))
seasonData = rep(1, length(ts1))
trend = list(
  name = "Trend",
  data = trendData,
  times = times,
  seasons = trendSeasons,
  timeKnots = trendTimeKnots,
  seasonalStructure = trendSeasonalStructure,
  lambdas = c(0.5, 0, 0)
)
season = list(
  name = "Yearly seasonality",
  data = seasonData,
  times = times,
  seasons = seasons,
  timeKnots = seasonTimeKnots,
  seasonalStructure = seasonalStructure,
  lambdas = c(10, 0, 0)
)
predictors = list(trend, season)

str1 = STR(
  data,
  predictors,
  confidence = c(0.99, 0.999),
  gap = 48,
  reltol = 0.00001
)
plot(str1)

################################################################

trendSeasonalStructure = list(segments = list(c(0, 1)), sKnots = list(c(1, 0)))
seasonalStructure = list(
  segments = list(c(0, 12)),
  sKnots = list(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, c(12, 0))
)
seasons = as.vector(cycle(ts1))
trendSeasons = rep(1, length(ts1))
times = as.vector(time(ts1))
data = as.vector(ts1)
trendTimeKnots = times
seasonTimeKnots = times
trendData = rep(1, length(ts1))
seasonData = rep(1, length(ts1))
trend = list(
  name = "Trend",
  data = trendData,
  times = times,
  seasons = trendSeasons,
  timeKnots = trendTimeKnots,
  seasonalStructure = trendSeasonalStructure,
  lambdas = c(0.5, 0, 0)
)
season = list(
  name = "Yearly seasonality",
  data = seasonData,
  times = times,
  seasons = seasons,
  timeKnots = seasonTimeKnots,
  seasonalStructure = seasonalStructure,
  lambdas = c(10, 0, 0)
)
predictors = list(trend, season)

str1 = STR(data, predictors, gap = 48, reltol = 0.0001)
plot(str1)

################################################################

trendSeasonalStructure = list(segments = list(c(0, 1)), sKnots = list(c(1, 0)))
seasonalStructure = list(
  segments = list(c(0, 12)),
  sKnots = list(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, c(12, 0))
)
seasons = as.vector(cycle(ts3))
trendSeasons = rep(1, length(ts3))
times = as.vector(time(ts3))
data = as.vector(ts3)
data[tail(seq_along(data), 24)] = NA
trendTimeKnots = times
seasonTimeKnots = times
trendData = rep(1, length(ts3))
seasonData = rep(1, length(ts3))
trend = list(
  name = "Trend",
  data = trendData,
  times = times,
  seasons = trendSeasons,
  timeKnots = trendTimeKnots,
  seasonalStructure = trendSeasonalStructure,
  lambdas = c(0.5, 0, 0)
)
season = list(
  name = "Yearly seasonality",
  data = seasonData,
  times = times,
  seasons = seasons,
  timeKnots = seasonTimeKnots,
  seasonalStructure = seasonalStructure,
  lambdas = c(10, 0, 0)
)
predictors = list(trend, season)

str1 = STR(data, predictors, gap = 24, reltol = 0.0001, confidence = 0.8)
plot(str1)

################################################################

# (b) Number of calls handled on weekdays between 7:00 am and 9:05 pm
# Five-minute call volume from March 3, 2003, to May 23, 2003
# in a large North American commercial bank.
callsOrig <- read.csv(
  "../externalTests/callcenter.csv",
  header = TRUE,
  sep = "\t"
)
callsSubset = callsOrig[, 2:(5 * 5)]
# calls <- msts(unlist(callsSubset), start=2003 + (31+28+2)/365.25, seasonal.periods = c(169, 169*5), ts.frequency = 365.25*169)
calls <- ts(
  unlist(callsSubset),
  start = 2003 + (31 + 28 + 2) / 365.25,
  frequency = 365.25 * 169
)
plot(calls)

trendSeasonalStructure = list(segments = list(c(0, 1)), sKnots = list(c(1, 0)))
trendSeasons = rep(1, length(calls))
times = as.vector(time(calls))
timeKnots = seq(min(times), max(times), length.out = 25)

sKnotsDays = as.list(seq(1, 169, length.out = 169))
seasonalStructureDays = list(segments = list(c(1, 169)), sKnots = sKnotsDays)
seasonsDays = seq_along(calls) %% 169 + 1

sKnotsWeeks = as.list(seq(0, 169 * 5, length.out = 13 * 5))
seasonalStructureWeeks = list(
  segments = list(c(0, 169 * 5)),
  sKnots = sKnotsWeeks
)
seasonsWeeks = seq_along(calls) %% (169 * 5) + 1

data = as.vector(calls)
trendData = rep(1, length(calls))
seasonData = rep(1, length(calls))

trend = list(
  data = trendData,
  times = times,
  seasons = trendSeasons,
  timeKnots = timeKnots,
  seasonalStructure = trendSeasonalStructure,
  lambdas = c(0.02, 0, 0)
)

seasonDays = list(
  data = seasonData,
  times = times,
  seasons = seasonsDays,
  timeKnots = seq(min(times), max(times), length.out = 25),
  seasonalStructure = seasonalStructureDays,
  lambdas = c(0, 11, 30)
)

seasonWeeks = list(
  data = seasonData,
  times = times,
  seasons = seasonsWeeks,
  timeKnots = seq(min(times), max(times), length.out = 25),
  seasonalStructure = seasonalStructureWeeks,
  lambdas = c(30, 500, 0.02)
)

predictors = list(trend, seasonDays, seasonWeeks)

tm = system.time({
  str1 = STRmodel(data, predictors, confidence = 0.95)
})
plot(str1)

# Error in { : task 1 failed - "CHOLMOD factorization was unsuccessful"
# In addition: Warning messages:
# 1: In .solve.dgC.chol(as(if (transpose) tx else t(x), "CsparseMatrix"),  :
# Cholmod warning 'matrix not positive definite' at file ../Supernodal/t_cholmod_super_numeric.c, line 729
# 2: In .solve.dgC.chol(as(if (transpose) tx else t(x), "CsparseMatrix"),  :
# Cholmod warning 'matrix not positive definite' at file ../Supernodal/t_cholmod_super_numeric.c, line 729
# 3: In .solve.dgC.chol(as(if (transpose) tx else t(x), "CsparseMatrix"),  :
# Cholmod warning 'matrix not positive definite' at file ../Supernodal/t_cholmod_super_numeric.c, line 729
# 4: In .solve.dgC.chol(as(if (transpose) tx else t(x), "CsparseMatrix"),  :
# Cholmod warning 'matrix not positive definite' at file ../Supernodal/t_cholmod_super_numeric.c, line 729

st = system.time({
  str = STR(
    data = data,
    predictors = predictors,
    confidence = NULL,
    reltol = 0.003,
    nFold = 4,
    gap = 169
  )
})
print(st)
plot(str)

################################################################

# Error in .solve.dgC.chol(as(if (transpose) tx else t(x), "CsparseMatrix"),  :
# CHOLMOD factorization was unsuccessful
# In addition: Warning message:
# In .solve.dgC.chol(as(if (transpose) tx else t(x), "CsparseMatrix"),  :
# Cholmod warning 'matrix not positive definite' at file ../Supernodal/t_cholmod_super_numeric.c, line 729

# trendSeasonalStructure = list(segments = list(c(0,1)), sKnots = list(c(1,0)))
# trendSeasons = rep(1, length(calls))
# times = as.vector(time(calls))
# timeKnots = seq(min(times), max(times), length.out=25)
#
# sKnotsDays = as.list(seq(1,169,length.out = 169))
# seasonalStructureDays = list(segments = list(c(1, 169)), sKnots = sKnotsDays)
# seasonsDays = seq_along(calls) %% 169 + 1
#
# sKnotsWeeks = as.list(c(seq(1,169,length.out=13),
#                       seq(1,169,length.out=13) + 169,
#                       seq(1,169,length.out=13) + 169*2,
#                       seq(1,169,length.out=13) + 169*3,
#                       seq(1,169,length.out=13) + 169*4))
#
# seasonalStructureWeeks = list(segments = list(c(1, 169),
#                                               c(1, 169) + 169,
#                                               c(1, 169) + 169*2,
#                                               c(1, 169) + 169*3,
#                                               c(1, 169) + 169*4),
#                               sKnots = sKnotsWeeks)
#
# seasonsWeeks = seq_along(calls) %% (169*5) + 1
#
# data = as.vector(calls)
# trendData = rep(1, length(calls))
# seasonData = rep(1, length(calls))
#
# trend = list(data = trendData, times = times, seasons = trendSeasons,
#              timeKnots = timeKnots,
#              seasonalStructure = trendSeasonalStructure, lambdas = c(0.02, 0, 0))
#
# seasonDays = list(data = seasonData, times = times, seasons = seasonsDays,
#                   timeKnots = seq(min(times), max(times), length.out=25),
#                   seasonalStructure = seasonalStructureDays, lambdas = c(0.1, 11, 30))
#
# seasonWeeks = list(data = seasonData, times = times, seasons = seasonsWeeks,
#                    timeKnots = seq(min(times), max(times), length.out=25),
#                    seasonalStructure = seasonalStructureWeeks, lambdas = c(30, 500, 0.02))
#
# predictors = list(trend, seasonDays, seasonWeeks)
#
#
# st = system.time({
#   str = STR(data = data, predictors = predictors, confidence = NULL, reltol = 0.01, nFold = 4, gap = 13)
# }); print(st)
# plot(str)

################################################################

VIC <- read.csv("../externalTests/VIC.csv", colClasses = rep("numeric", 4))
Frankston <- read.csv(
  "../externalTests/Frankston.csv",
  colClasses = rep("numeric", 3)
)
colnames(Frankston)[3] <- "TempF"
Melbourne <- read.csv(
  "../externalTests/Melbourne.csv",
  colClasses = rep("numeric", 3)
)
colnames(Melbourne)[3] <- "TempM"
holidays <- read.table("../externalTests/VIC_public_holidays.csv", quote = "\"")
colnames(holidays) <- "Date"
holidays[, 1] <- as.Date(holidays[, 1], format = "%d/%m/%Y")
holidays$Holidays <- T
temp = merge(Melbourne, Frankston, sort = F, all = T)
data = merge(VIC, temp, sort = F, all.x = T)
data[, "Date"] <- as.Date(data[, "Date"], origin = "1899-12-30")

data = merge(data, holidays, sort = F, all.x = T)
data[is.na(data[, "Holidays"]), "Holidays"] = F
data = data[order(data[, "Date"], data[, "Period"]), ]
rownames(data) = seq_len(nrow(data))
data$Weekday = as.integer(factor(
  weekdays(data[, "Date"], abbreviate = T),
  levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
))

data$Time = seq_along(data[, "Date"])
data$DailySeasonality = data[, "Period"] - 1
data$WeeklySeasonality = (data[, "Weekday"] - 1) * 48 + data[, "Period"] - 1
data$WorkingDay = (!data[, "Holidays"]) & (data[, "Weekday"] %in% 1:5)

WorkingDayIn48 = c(data$WorkingDay[-(1:48)], rep(NA, 48))
WorkingDay48Ago = c(
  rep(NA, 48),
  data$WorkingDay[-((length(data$WorkingDay) - 47):length(data$WorkingDay))]
)

data$WorkingDayBeforeHoliday = data$WorkingDay & (!WorkingDayIn48)
data$WorkingDayAfterHoliday = data$WorkingDay & (!WorkingDay48Ago)
data$HolidayBeforeWorkingDay = (!data$WorkingDay) & WorkingDayIn48
data$HolidayAfterWorkingDay = (!data$WorkingDay) & WorkingDay48Ago
data$HolidayAlone = (!data$WorkingDay) & (!WorkingDay48Ago) & (!WorkingDayIn48)
data$WorkinDayAlone = (data$WorkingDay) & (WorkingDay48Ago) & (WorkingDayIn48)

data$WDSeasonality = data$DailySeasonality + (!data$WorkingDay) * 100

Data2 = data[, c(
  "OperationalLessIndustrial",
  "TempM",
  "TempF",
  "Time",
  "DailySeasonality",
  "WeeklySeasonality",
  "WDSeasonality"
)]
colnames(Data2) <- c(
  "Demand",
  "TempM",
  "TempF",
  "Time",
  "DailySeasonality",
  "WeeklySeasonality",
  "WDSeasonality"
)

myData = Data2[481:6000, ]

TrendSeasonalStructure = list(segments = list(c(0, 1)), sKnots = list(c(1, 0)))
DailySeasonalStructure = list(
  segments = list(c(0, 48)),
  sKnots = c(as.list(1:47), list(c(48, 0)))
)
WeeklySeasonalStructure = list(
  segments = list(c(0, 336)),
  sKnots = c(as.list(1:335), list(c(336, 0)))
)
WeeklySeasonalStructure4 = list(
  segments = list(c(0, 336)),
  sKnots = c(as.list(seq(4, 332, 4)), list(c(336, 0)))
)
WeeklySeasonalStructure2 = list(
  segments = list(c(0, 336)),
  sKnots = c(as.list(seq(2, 334, 2)), list(c(336, 0)))
)
WDSeasonalStructure = list(
  segments = list(c(0, 48), c(100, 148)),
  sKnots = c(as.list(c(1:47, 101:147)), list(c(0, 48, 100, 148)))
)

TrendSeasons = rep(1, nrow(myData))
DailySeasons = myData$DailySeasonality
WeeklySeasons = myData$WeeklySeasonality
WDSeasons = myData$WDSeasonality

Times = myData$Time

Data = myData$Demand
TempM = myData$TempM
TempM2 = (myData$TempM)^2

TrendTimeKnots = seq(
  from = stR:::first(Times),
  to = stR:::last(Times),
  length.out = 116
)
SeasonTimeKnots = seq(
  from = stR:::first(Times),
  to = stR:::last(Times),
  length.out = 24
)

TrendData = rep(1, length(Times))
SeasonData = rep(1, length(Times))

Trend = list(
  name = "Trend",
  data = TrendData,
  times = Times,
  seasons = TrendSeasons,
  timeKnots = TrendTimeKnots,
  seasonalStructure = TrendSeasonalStructure,
  lambdas = c(100, 0, 0)
)

WDSeason = list(
  name = "Dayly seas",
  data = SeasonData,
  times = Times,
  seasons = WDSeasons,
  timeKnots = SeasonTimeKnots,
  seasonalStructure = WDSeasonalStructure,
  lambdas = c(3, 0, 50)
)

TrendTempM = list(
  name = "Trend temp Mel",
  data = TempM,
  times = Times,
  seasons = TrendSeasons,
  timeKnots = TrendTimeKnots,
  seasonalStructure = TrendSeasonalStructure,
  lambdas = c(10, 0, 0)
)
TrendTempM2 = list(
  name = "Trend temp Mel^2",
  data = TempM2,
  times = Times,
  seasons = TrendSeasons,
  timeKnots = TrendTimeKnots,
  seasonalStructure = TrendSeasonalStructure,
  lambdas = c(10, 0, 0)
)

Predictors = list(Trend, WDSeason, TrendTempM, TrendTempM2)

# str = STRmodel(data = Data, predictors = Predictors, reportDimensionsOnly = T)
# st = system.time({ str = STRmodel(data = Data, predictors = Predictors, confidence = 0.95) }); print(st)

st = system.time({
  str = STR(
    data = Data,
    predictors = Predictors,
    confidence = 0.95,
    gapCV = 48 * 7
  )
  # str = STR(data = Data, predictors = Predictors, confidence = NULL, gapCV = 48*7)
})
print(st)

plot(str)

################################################################

StaticTempM <- list(
  name = "Temp Mel",
  data = TempM,
  times = Times,
  seasons = NULL,
  timeKnots = NULL,
  seasonalStructure = NULL,
  lambdas = c(0, 0, 0)
)
StaticTempM2 <- list(
  name = "Temp Mel^2",
  data = TempM2,
  times = Times,
  seasons = NULL,
  timeKnots = NULL,
  seasonalStructure = NULL,
  lambdas = c(0, 0, 0)
)
Predictors2 <- list(Trend, WDSeason, StaticTempM, StaticTempM2)

elec.fit.2 <- STR(
  data = Data,
  predictors = Predictors2,
  confidence = 0.95,
  gapCV = 48 * 7
)

plot(
  elec.fit.2,
  xTime = as.Date("2000-01-11") + ((Times - 1) / 48 - 10),
  forecastPanels = NULL
)

################################################################

# x <- msts(c(log(as.vector(taylor)),rep(NA, 336)), seasonal.periods=c(48,336), ts.frequency=48*7*52.25, start=2000+22/52)
x <- msts(
  log(head(as.vector(taylor), 336 * 4)),
  seasonal.periods = c(48, 336),
  ts.frequency = 48 * 7 * 52.25,
  start = 2000 + 22 / 52
)
plot(x)

# 48 - daily seasonality (half hour granularity)
# 336 - weekly seasonality

str.msts = AutoSTR(x, gapCV = 48, reltol = 0.01, confidence = 0.95)
plot(str.msts)

str.msts.2 = AutoSTR(
  x,
  gapCV = 48,
  reltol = 0.0005,
  confidence = 0.95,
  lambdas = str.msts$input$lambdas
)
plot(str.msts.2)

################################################################

# (c) Turkish electricity demand data.
# Daily data from 1 January 2000 to 31 December 2008.
# telec <- read.csv("http://robjhyndman.com/data/turkey_elec.csv")
telec <- read.csv("../externalTests/turkey_elec.csv")
# telec <- msts(telec, start=2000, seasonal.periods = c(7,354.37,365.25))
telec <- msts(
  head(telec, 365.25 * 4),
  start = 2000,
  seasonal.periods = c(7, 354.37, 365.25)
)
plot(telec)

telec.msts = AutoSTR(telec, gapCV = 14, reltol = 0.01, confidence = NULL)
plot(telec.msts)

# Takes too long
# Best parameters:
# 0.3054866 0.0000000 0.0000000
# 0.7882012 0.2436795 5.8281938
# 0.06431178 0.64732929 3.22534876
# 3.640675 4.842222 1.880013
# telec.msts.2 = STR(telec, gapCV = 14, reltol = 0.01, confidence = NULL, lambdas = telec.msts$input$lambdas, nsKnots = c(7,354,365))
# plot(telec.msts.2)

################################################################

str.taylor = AutoSTR(taylor, trace = F)
plot(str.taylor)

################################################################

taylor.msts <- msts(
  log(head(as.vector(taylor), 336 * 4)),
  seasonal.periods = c(48, 48 * 7, 48 * 7 * 52.25),
  start = 2000 + 22 / 52
)
taylor.fit = AutoSTR(
  taylor.msts,
  gapCV = 48,
  reltol = 0.001,
  confidence = 0.95,
  trace = F
)
plot(taylor.fit)

####

taylor.fit2 = AutoSTR(taylor.msts, trace = F)
plot(taylor.fit2)

################################################################
