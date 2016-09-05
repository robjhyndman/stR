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
  l = length(object$input$data)
  n = length(object$output$predictors) + 2 # predictors, original data and random component
  m = matrix(0, l, n)
  m[,1] = as.vector(object$input$data)
  m[,ncol(m)] = as.vector(object$output$random$data)
  names = rep("", ncol(m))
  names[c(1, ncol(m))] = c("Original", "Random")
  for(i in seq_along(object$output$predictors)) {
    m[,i+1] = object$output$predictors[[i]]$data
    names[i+1] = object$input$predictors[[i]]$name
  }
  colnames(m) = names
  if("ts" %in% class(object$input$data)) {
    p = tsp(object$input$data)
    result = ts(m, start = p[1], end = p[2], frequency = p[3])
  } else {
    result = ts(m)
  }
  return(result)
}
