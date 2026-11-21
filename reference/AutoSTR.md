# Automatic STR decomposition for time series data

Automatically selects parameters for an STR decomposition of time series
data. The time series should be of class `ts` or `msts`.

## Usage

``` r
AutoSTR(
  data,
  robust = FALSE,
  gapCV = NULL,
  lambdas = NULL,
  reltol = 0.001,
  confidence = NULL,
  nsKnots = NULL,
  trace = FALSE
)
```

## Arguments

- data:

  A time series of class `ts` or `msts`.

- robust:

  When `TRUE`, Robust STR decomposition is used. Default is `FALSE`.

- gapCV:

  An optional parameter defining the length of the sequence of skipped
  values in the cross validation procedure.

- lambdas:

  An optional parameter. A structure which replaces lambda parameters
  provided with predictors. It is used as either a starting point for
  the optimisation of parameters or as the exact model parameters.

- reltol:

  An optional parameter which is passed directly to
  [`optim()`](https://rdrr.io/r/stats/optim.html) when optimising the
  parameters of the model.

- confidence:

  A vector of percentiles giving the coverage of confidence intervals.
  It must be greater than 0 and less than 1. If `NULL`, no confidence
  intervals are produced.

- nsKnots:

  An optional vector parameter, defining the number of seasonal knots
  (per period) for each sesonal component.

- trace:

  When `TRUE`, tracing is turned on.

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

- **optim.CV.MSE** – best cross validated Mean Squared Error (n-fold)
  achieved during minimisation procedure.

- **nFold** – the input `nFold` parameter.

- **gapCV** – the input `gapCV` parameter.

- **method** – always contains string `"AutoSTR"` for this function.

## References

Dokumentov, A., and Hyndman, R.J. (2022) STR: Seasonal-Trend
decomposition using Regression, *INFORMS Journal on Data Science*, 1(1),
50-62. <https://robjhyndman.com/publications/str/>

## See also

[`STR`](https://pkg.robjhyndman.com/stR/reference/STR.md)

## Author

Alexander Dokumentov

## Examples

``` r
# \donttest{
# Decomposition of a multiple seasonal time series
decomp <- AutoSTR(calls)
plot(decomp)


# Decomposition of a monthly time series
decomp <- AutoSTR(log(grocery))
plot(decomp)

# }
```
