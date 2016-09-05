#' @name components
#' @rdname components
#'
#' @title Extracts components as time series from the result of decomposition
#' @description \code{components} extracts components as time series from the result of STR decomposition.
#' @seealso \code{\link{STRmodel}}, \code{\link{RSTRmodel}}, \code{\link{STR}}, \code{\link{AutoSTR}}
#' @param object Result of STR decomposition.
#' @author Alexander Dokumentov
#' @examples
#' \dontrun{
#'
#' fit <- AutoSTR(log(grocery))
#' comp = components(fit)
#' plot(comp)
#'
#' }
#' @export

components = function(object)
{
  return(NULL)
}

