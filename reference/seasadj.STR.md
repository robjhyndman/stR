# Seasonal adjustment based on STR

`seasadj.STR` extracts seasonally adjusted data by removing the seasonal
components from the result of STR decomposition.

## Usage

``` r
# S3 method for class 'STR'
seasadj(object, include = c("Trend", "Random"), ...)
```

## Arguments

- object:

  Result of STR decomposition.

- include:

  Vector of component names to include in the result. The default is
  `c("Trend", "Random")`.

- ...:

  Other arguments not currently used.

## See also

[`STRmodel`](https://pkg.robjhyndman.com/stR/reference/STRmodel.md),
[`RSTRmodel`](https://pkg.robjhyndman.com/stR/reference/RSTRmodel.md),
[`STR`](https://pkg.robjhyndman.com/stR/reference/STR.md),
[`AutoSTR`](https://pkg.robjhyndman.com/stR/reference/AutoSTR.md)

## Author

Alexander Dokumentov

## Examples

``` r
# \donttest{
fit <- AutoSTR(log(grocery))
plot(seasadj(fit))

# }
```
