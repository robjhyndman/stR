#' @importFrom stats tsp
#' @importFrom stats tsp<-
#' @importFrom stats ts

#' @name components
#' @rdname components
#'
#' @title Extract STR components
#' @description \code{components} extracts components as time series from the result of an STR decomposition.
#' @seealso \code{\link{STRmodel}}, \code{\link{RSTRmodel}}, \code{\link{STR}}, \code{\link{AutoSTR}}
#' @param object Result of STR decomposition.
#' @author Alexander Dokumentov
#' @examples
#' \dontrun{
#' 
#' fit <- AutoSTR(log(grocery))
#' comp <- components(fit)
#' plot(comp)
#' }
#' @export

components <- function(object)
{
  # Set up matrix for components
  l <- length(object$input$data)
  n <- length(object$output$predictors) + 2 # predictors, original data and random component
  m <- matrix(0, l, n)

  # Add data to first column and random to last column
  m[,1] <- as.vector(object$input$data)
  m[,ncol(m)] <- as.vector(object$output$random$data)
  names <- rep("", ncol(m))
  names[c(1, ncol(m))] = c("Data", "Random")

  # Add remaining components
  for(i in seq_along(object$output$predictors)) {
    m[,i+1] <- object$output$predictors[[i]]$data
    names[i+1] <- object$input$predictors[[i]]$name
  }
  colnames(m) <- names

  # Return result with ts attributes
  m <- ts(m)
  if("ts" %in% class(object$input$data))
    tsp(m) <- tsp(object$input$data)
  return(m)
}
