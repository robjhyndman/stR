#' @name seasadj.STR
#' @rdname seasadj.STR
#'
#' @title Seasonal adjustment
#' @description \code{seasadj.STR} extracts seasonally adjusted data by removing the seasonal components from the result of STR decomposition.
#' @seealso \code{\link{STRmodel}}, \code{\link{RSTRmodel}}, \code{\link{STR}}, \code{\link{AutoSTR}}
#' @param object Result of STR decomposition.
#' @author Alexander Dokumentov
#' @examples
#' \dontrun{
#'
#' fit <- AutoSTR(log(grocery))
#' plot(seasadj(fit))
#'
#' }
#' @export

seasadj.STR = function(object)
{
  return(NULL)
}

