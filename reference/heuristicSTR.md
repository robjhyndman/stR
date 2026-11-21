# Automatic STR decomposition with heuristic search of the parameters

Automatically selects parameters (lambda coefficients) for an STR
decomposition of time series data. Heuristic approach can give a better
estimate compare to a standard optmisaton methods used in
[`STR`](https://pkg.robjhyndman.com/stR/reference/STR.md).

If a parallel backend is registered for use before `STR` call,
`heuristicSTR` will use it for n-fold cross validation computations.

## Usage

``` r
heuristicSTR(
  data,
  predictors,
  confidence = NULL,
  lambdas = NULL,
  pattern = extractPattern(predictors),
  nFold = 5,
  reltol = 0.005,
  gapCV = 1,
  solver = c("Matrix", "cholesky"),
  trace = FALSE,
  ratioGap = 1e+12,
  relCV = 0.01
)
```

## Arguments

- data:

  Time series or a vector of length ***L***.

- predictors:

  List of predictors.  
  According to the paradigm of this implementation, the trend, the
  seasonal components, the flexible predictors and the seasonal
  predictors are all presented in the same form (as predictors) and must
  be described in this list.  
  Every predictor is a list of the following structures:  

  - **data** – vector of length ***L*** (length of input data, see
    above). For trend or for a seasonal component it is a vector of
    ones. For a flexible or a seasonal predictor it is a vector of the
    predictor's data.

  - **times** – vector of length ***L*** of times of observations.

  - **seasons** – vector of length ***L***. It is a vector of ones for a
    trend or a flexible predictor. It is vector assigning seasons to
    every observation (for a seasonal component or a seasonal
    predictor). Seasons can be fractional for observations in between
    seasons.

  - **timeKnots** – vector of times (time knots) where knots are
    positioned (for a seasonal component or a seasonal predictor a few
    knots have the same time; every knot is represented by time and
    season). Usually this vector coincides with **times** vector
    described above, or **timeKnots** is a subset of **times** vector.

  - **seasonalStructure** – describes seasonal topology (which can have
    complex structure) and seasonal knots.The seasonal topology is
    described by a list of segments and seasonal knots, which are
    positioned inside the segments, on borders of the segments or, when
    they are on on borders, they can connect two or more segments.  
    **seasonalStructure** is a list of two elements:  

    - **segments** – a list of vectors representing segments. Each
      vector must contain two ordered real values which represent left
      and right borders of a segment. Segments should not intersect
      (inside same predictor).

    - **sKnots** – a list of real values (vectors of length one) or
      vectors of lengths two or greater (seasonal knots) defining
      seasons of the knots (every knot is represented by time and
      season). All real values must belong (be inside or on border of)
      segments listed in **segments**. If a few values represent a
      single seasonal knot then all these values must be on borders of
      some segments (or a single segment). In this case they represent a
      seasonal knot which connects a few segments (or both sides of one
      segment).

  - **lambdas** – a vector with three values representing lambda
    (smoothing) parameters (time-time, season-season, time-season
    flexibility parameters) for this predictor.

- confidence:

  A vector of percentiles giving the coverage of confidence intervals.
  It must be greater than 0 and less than 1. If `NULL`, no confidence
  intervals are produced.

- lambdas:

  An optional parameter. A structure which replaces lambda parameters
  provided with predictors. It is used as either a starting point for
  the optimisation of parameters or as the exact model parameters.

- pattern:

  An optional parameter which has the same structure as `lambdas`
  although with a different meaning. All zero values correspond to
  lambda (smoothing) parameters which will not be estimated.

- nFold:

  An optional parameter setting the number of folds for cross
  validation.

- reltol:

  An optional parameter which is passed directly to
  [`optim()`](https://rdrr.io/r/stats/optim.html) when optimising the
  parameters of the model.

- gapCV:

  An optional parameter defining the length of the sequence of skipped
  values in the cross validation procedure.

- solver:

  A vector with two string values. The only supported combinations are:
  c("Matrix", "cholesky") (default), and c("Matrix", "qr"). The
  parameter is used to specify a particular library and method to solve
  the minimisation problem during STR decompositon.

- trace:

  When `TRUE`, tracing is turned on.

- ratioGap:

  Ratio to define hyperparameter bounds for one-dimensional search.

- relCV:

  Minimum improvement required after all predictors tried. It is used to
  exit heuristic search of lambda parameters.

## Value

A structure containing input and output data. It is an **S3** class
`STR`, which is a list with the following components:

- **output** – contains decomposed data. It is a list of three
  components:

  - **predictors** – a list of components where each component
    corresponds to the input predictor. Every such component is a list
    containing the following:

    - **data** – fit/forecast for the corresponding predictor (trend,
      seasonal component, flexible or seasonal predictor).

    - **beta** – beta coefficients of the fit of the coresponding
      predictor.

    - **lower** – optional (if requested) matrix of lower bounds of
      confidence intervals.

    - **upper** – optional (if requested) matrix of upper bounds of
      confidence intervals.

  - **random** – a list with one component **data**, which contains
    residuals of the model fit.

  - **forecast** – a list with two components:

    - **data** – fit/forecast for the model.

    - **beta** – beta coefficients of the fit.

    - **lower** – optional (if requested) matrix of lower bounds of
      confidence intervals.

    - **upper** – optional (if requested) matrix of upper bounds of
      confidence intervals.

- **input** – input parameters and lambdas used for final calculations.

  - **data** – input data.

  - **predictors** - input predictors.

  - **lambdas** – smoothing parameters used for final calculations (same
    as input lambdas for STR method).

- **cvMSE** – optional cross validated (leave one out) Mean Squared
  Error.

- **optim.CV.MSE** or **optim.CV.MAE** – best cross validated Mean
  Squared Error or Mean Absolute Error (n-fold) achieved during
  minimisation procedure.

- **nFold** – the input `nFold` parameter.

- **gapCV** – the input `gapCV` parameter.

- **method** – contains strings `"STR"` or `"RSTR"` depending on used
  method.

## References

Dokumentov, A., and Hyndman, R.J. (2022) STR: Seasonal-Trend
decomposition using Regression, *INFORMS Journal on Data Science*, 1(1),
50-62. <https://robjhyndman.com/publications/str/>

## See also

[`STR`](https://pkg.robjhyndman.com/stR/reference/STR.md)
[`STRmodel`](https://pkg.robjhyndman.com/stR/reference/STRmodel.md)
[`AutoSTR`](https://pkg.robjhyndman.com/stR/reference/AutoSTR.md)

## Author

Alexander Dokumentov

## Examples

``` r
# \donttest{
TrendSeasonalStructure <- list(
  segments = list(c(0, 1)),
  sKnots = list(c(1, 0))
)
WDSeasonalStructure <- list(
  segments = list(c(0, 48), c(100, 148)),
  sKnots = c(as.list(c(1:47, 101:147)), list(c(0, 48, 100, 148)))
)

TrendSeasons <- rep(1, nrow(electricity))
WDSeasons <- as.vector(electricity[, "WorkingDaySeasonality"])

Data <- as.vector(electricity[, "Consumption"])
Times <- as.vector(electricity[, "Time"])
TempM <- as.vector(electricity[, "Temperature"])
TempM2 <- TempM^2

TrendTimeKnots <- seq(from = head(Times, 1), to = tail(Times, 1), length.out = 116)
SeasonTimeKnots <- seq(from = head(Times, 1), to = tail(Times, 1), length.out = 24)

TrendData <- rep(1, length(Times))
SeasonData <- rep(1, length(Times))

Trend <- list(
  name = "Trend",
  data = TrendData,
  times = Times,
  seasons = TrendSeasons,
  timeKnots = TrendTimeKnots,
  seasonalStructure = TrendSeasonalStructure,
  lambdas = c(1500, 0, 0)
)
WDSeason <- list(
  name = "Dayly seas",
  data = SeasonData,
  times = Times,
  seasons = WDSeasons,
  timeKnots = SeasonTimeKnots,
  seasonalStructure = WDSeasonalStructure,
  lambdas = c(0.003, 0, 240)
)
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
Predictors <- list(Trend, WDSeason, StaticTempM, StaticTempM2)

elec.fit <- heuristicSTR(
  data = Data,
  predictors = Predictors,
  gapCV = 48 * 7
)

plot(elec.fit,
  xTime = as.Date("2000-01-11") + ((Times - 1) / 48 - 10),
  forecastPanels = NULL
)


########################################

TrendSeasonalStructure <- list(
  segments = list(c(0, 1)),
  sKnots = list(c(1, 0))
)
DailySeasonalStructure <- list(
  segments = list(c(0, 48)),
  sKnots = c(as.list(1:47), list(c(48, 0)))
)
WeeklySeasonalStructure <- list(
  segments = list(c(0, 336)),
  sKnots = c(as.list(seq(4, 332, 4)), list(c(336, 0)))
)
WDSeasonalStructure <- list(
  segments = list(c(0, 48), c(100, 148)),
  sKnots = c(as.list(c(1:47, 101:147)), list(c(0, 48, 100, 148)))
)

TrendSeasons <- rep(1, nrow(electricity))
DailySeasons <- as.vector(electricity[, "DailySeasonality"])
WeeklySeasons <- as.vector(electricity[, "WeeklySeasonality"])
WDSeasons <- as.vector(electricity[, "WorkingDaySeasonality"])

Data <- as.vector(electricity[, "Consumption"])
Times <- as.vector(electricity[, "Time"])
TempM <- as.vector(electricity[, "Temperature"])
TempM2 <- TempM^2

TrendTimeKnots <- seq(from = head(Times, 1), to = tail(Times, 1), length.out = 116)
SeasonTimeKnots <- seq(from = head(Times, 1), to = tail(Times, 1), length.out = 24)
SeasonTimeKnots2 <- seq(from = head(Times, 1), to = tail(Times, 1), length.out = 12)

TrendData <- rep(1, length(Times))
SeasonData <- rep(1, length(Times))

Trend <- list(
  name = "Trend",
  data = TrendData,
  times = Times,
  seasons = TrendSeasons,
  timeKnots = TrendTimeKnots,
  seasonalStructure = TrendSeasonalStructure,
  lambdas = c(1500, 0, 0)
)
WSeason <- list(
  name = "Weekly seas",
  data = SeasonData,
  times = Times,
  seasons = WeeklySeasons,
  timeKnots = SeasonTimeKnots2,
  seasonalStructure = WeeklySeasonalStructure,
  lambdas = c(0.8, 0.6, 100)
)
WDSeason <- list(
  name = "Dayly seas",
  data = SeasonData,
  times = Times,
  seasons = WDSeasons,
  timeKnots = SeasonTimeKnots,
  seasonalStructure = WDSeasonalStructure,
  lambdas = c(0.003, 0, 240)
)
TrendTempM <- list(
  name = "Trend temp Mel",
  data = TempM,
  times = Times,
  seasons = TrendSeasons,
  timeKnots = TrendTimeKnots,
  seasonalStructure = TrendSeasonalStructure,
  lambdas = c(1e7, 0, 0)
)
TrendTempM2 <- list(
  name = "Trend temp Mel^2",
  data = TempM2,
  times = Times,
  seasons = TrendSeasons,
  timeKnots = TrendTimeKnots,
  seasonalStructure = TrendSeasonalStructure,
  lambdas = c(0.01, 0, 0)
) # Starting parameter is too far from the optimal value
Predictors <- list(Trend, WSeason, WDSeason, TrendTempM, TrendTempM2)

elec.fit <- heuristicSTR(
  data = Data,
  predictors = Predictors,
  gapCV = 48 * 7
)

plot(elec.fit,
  xTime = as.Date("2000-01-11") + ((Times - 1) / 48 - 10),
  forecastPanels = NULL
)


plotBeta(elec.fit, predictorN = 4)

plotBeta(elec.fit, predictorN = 5)

# }
```
