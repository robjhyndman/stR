suppressPackageStartupMessages(library(stR))
suppressPackageStartupMessages(library(dplyr))
# suppressPackageStartupMessages(library(lubridate))

# split the time series data into times, seasons and data
# Date set is 115 days of electical usage starting Jan, 2000
# with 48 observations per day
# 48 - one day
# 336 - one week

data(electricity)

# define Data sources
Data <- as.vector(electricity[,"Consumption"])
Times <- as.vector(electricity[,"Time"])
TempM <- as.vector(electricity[,"Temperature"])
TempM2 <- TempM^2

# define the trend and seasonal structure
TrendSeasonalStructure <- list(segments = list(c(0,1)),
                               sKnots = list(c(1,0)))
DailySeasonalStructure <- list(segments = list(c(0,48)),
                               sKnots = c(as.list(1:47), list(c(48,0))))
WeeklySeasonalStructure <- list(segments = list(c(0,336)),
                                sKnots = c(as.list(seq(4,332,4)), list(c(336,0))))
WDSeasonalStructure <- list(segments = list(c(0,48), c(100,148)),
                            sKnots = c(as.list(c(1:47,101:147)), list(c(0,48,100,148))))

# set up the seasons
TrendSeasons <- rep(1, nrow(electricity))
DailySeasons <- as.vector(electricity[,"DailySeasonality"])
WeeklySeasons <- as.vector(electricity[,"WeeklySeasonality"])
WDSeasons <- as.vector(electricity[,"WorkingDaySeasonality"])


# set up time knots
TrendTimeKnots <- seq(from = head(Times, 1),
                      to = tail(Times, 1),
                      # length.out = 116*4)
                      length.out = 116)
SeasonTimeKnots <- seq(from = head(Times, 1),
                       to = tail(Times, 1),
                       # length.out = 24*4)
                       length.out = 24)
SeasonTimeKnots2 <- seq(from = head(Times, 1),
                        to = tail(Times, 1),
                        # length.out = 12*4)
                        length.out = 12)

# trend and season data arrays
TrendData <- rep(1, length(Times))
SeasonData <- rep(1, length(Times))

# trend and season structures
Trend <- list(name = "Trend",
              data = TrendData,
              times = Times,
              seasons = TrendSeasons,
              timeKnots = TrendTimeKnots,
              seasonalStructure = TrendSeasonalStructure,
              lambdas = c(1500,0,0))
WSeason <- list(name = "Weekly seas",
                data = SeasonData,
                times = Times,
                seasons = WeeklySeasons,
                timeKnots = SeasonTimeKnots2,
                seasonalStructure = WeeklySeasonalStructure,
                lambdas = c(0.8,0.6,100))
WDSeason <- list(name = "Dayly seas",
                 data = SeasonData,
                 times = Times,
                 seasons = WDSeasons,
                 timeKnots = SeasonTimeKnots,
                 seasonalStructure = WDSeasonalStructure,
                 lambdas = c(0.003,0,240))
TrendTempM <- list(name = "Trend temp Mel",
                   data = TempM,
                   times = Times,
                   seasons = TrendSeasons,
                   timeKnots = TrendTimeKnots,
                   seasonalStructure = TrendSeasonalStructure,
                   lambdas = c(11200000,0,0))
TrendTempM2 <- list(name = "Trend temp Mel^2",
                    data = TempM2,
                    times = Times,
                    seasons = TrendSeasons,
                    timeKnots = TrendTimeKnots,
                    seasonalStructure = TrendSeasonalStructure,
                    lambdas = c(5000000,0,0))

# build the predictors
Predictors <- list(Trend, WSeason, WDSeason, TrendTempM, TrendTempM2)
# Predictors <- list(Trend, WDSeason, TrendTempM, TrendTempM2)

# blank one week of data
Data[5184:5520] <- NA

# calculate the decomposition
tm = system.time({
elec.fit <- STR(data = Data,
                predictors = Predictors,
                # confidence = 0.95,
                gapCV = 48*7,
                # gapCV = 1,
                # solver = c("iterative", "cg-chol"),
                # solver = c("iterative", "cg"),
                # solver = c("iterative", "lsmr-chol"),
                # solver = c("iterative", "lsmr"),
                iterControl = list(maxiter = 400, tol = 1e-5),
                trace = TRUE)
}); print(tm)


elec.fit.1 <- STRmodel(data = Data,
                predictors = Predictors,
                # confidence = 0.95,
                # gapCV = 1,
                # solver = c("iterative", "cg-chol"),
                # solver = c("iterative", "cg"),
                # solver = c("iterative", "lsmr-chol"),
                # solver = c("iterative", "lsmr"),
                # iterControl = list(maxiter = 400, tol = 1e-7),
                trace = TRUE)


# plot the results
plot(elec.fit,
     xTime = as.Date("2000-01-11") + ((Times-1)/48-10))
# ,
#      forecastPanels = 7)

elec.fit$optim.CV.MSE

# save the result
# save(elec.fit, file = "subset of complex seasonality with regressors.RData")


# tm = system.time({
#   elec.fit.1 <- STRmodel(data = Data,
#                   predictors = Predictors
#                   # solver = c("Matrix", "cholesky")
#                   # confidence = 0.95
#                   )
# }); print(tm)

plotBeta(elec.fit, predictorN = 3, dim = 1)
