#' @name seasadj.STR
#' @rdname seasadj.STR
#'
#' @title Seasonal adjustment based on STR
#' @description \code{seasadj.STR} extracts seasonally adjusted data by removing the seasonal components from the result of STR decomposition.
#' @seealso \code{\link{STRmodel}}, \code{\link{RSTRmodel}}, \code{\link{STR}}, \code{\link{AutoSTR}}
#' @param object Result of STR decomposition.
#' @param include Vector of component names to include in the result. The default is \code{c("Trend", "Random")}.
#' @param ... Other arguments not currently used.
#' @author Alexander Dokumentov
#' @examples
#' \donttest{
#' fit <- AutoSTR(log(grocery))
#' plot(seasadj(fit))
#' }
#' @export

seasadj.STR <- function(object, include = c("Trend", "Random"), ...) {
  # Extract all components
  compTs <- components(object)

  # Find trend
  trendName <- colnames(compTs)[2]
  if (is.null(trendName) || is.na(trendName) || nchar(trendName) == 0) {
    warning("Trend component is not specified by name, using the first component as the Trend component.")
    colnames(compTs)[2] <- "Trend"
  }

  # Check all components are available
  for (name in include[!(include %in% colnames(compTs))]) {
    warning(paste(name, "is not one of the components of the decomposion, skipping..."))
  }

  # Add together the components listed in include argument.
  result <- NULL
  for (i in include[include %in% colnames(compTs)]) {
    if (is.null(result)) {
      result <- compTs[, i]
    } else {
      result <- result + compTs[, i]
    }
  }

  # Return result
  return(result)
}

# Export seasadj from the forecast package
#' @export
forecast::seasadj
