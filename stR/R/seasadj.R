#' @name seasadj.STR
#' @rdname seasadj.STR
#'
#' @title Seasonal adjustment
#' @description \code{seasadj.STR} extracts seasonally adjusted data by removing the seasonal components from the result of STR decomposition.
#' @seealso \code{\link{STRmodel}}, \code{\link{RSTRmodel}}, \code{\link{STR}}, \code{\link{AutoSTR}}
#' @param object Result of STR decomposition.
#' @param include Vecor of component names to include in the result. The default is \code{c("Trend", "Random")}.
#' @author Alexander Dokumentov
#' @examples
#' \dontrun{
#'
#' fit <- AutoSTR(log(grocery))
#' plot(stR::seasadj(fit))
#'
#' }
#' @export

seasadj.STR = function(object, include = c("Trend", "Random"))
{
  compTs = components(object)
  trendName = colnames(compTs)[2]
  if(is.null(trendName) || is.na(trendName) || nchar(trendName) == 0) {
    warning("Trend component is not specified by name, using the first component as the Trend component.")
    colnames(compTs)[2] = "Trend"
  }
  for(name in include[!(include %in% colnames(compTs))]) {
    warning(paste(name, "is not one of the components of the decomposion, skipping..."))
  }
  result = NULL
  for(i in include[include %in% colnames(compTs)]) {
    if(is.null(result)) {
      result = compTs[,i]
    } else {
      result = result + compTs[,i]
    }
  }
  return(result)
}

#' @name seasadj
#' @rdname seasadj.STR
#' @export seasadj
seasadj <- function(object, ...) UseMethod("seasadj", object)

