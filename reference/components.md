# Extract STR components

`components` extracts components as time series from the result of an
STR decomposition.

## Usage

``` r
components(object)
```

## Arguments

- object:

  Result of STR decomposition.

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
comp <- components(fit)
plot(comp)

# }
```
