#' @title Automatic STR decomposition for time series data
#' @description Automatically selects parameters for an STR decomposition of time series data.
#' The time series should be of class \code{ts} or \code{msts}.
#'
#' If a parallel backend is registered for use before \code{AutoSTR.ts} call,
#' \code{AutoSTR.ts} will use it for n-fold cross validation computations.
#' @seealso \code{\link{AutoSTR}}
#' @param data A time series of class \code{ts} or \code{msts}.
#' @inheritParams gapCV
#' @inheritParams lambdas
#' @inheritParams reltol
#' @inheritParams confidence
#' @param nsKnots An optional vector parameter, defining the number of seasonal knots (per period) for each sesonal component.
#' @inheritParams trace
#' @templateVar class STR
#' @templateVar topLevel1 \item \strong{cvMSE} -- optional cross validated (leave one out) Mean Squared Error.
#' @templateVar topLevel2 \item \strong{optim.CV.MSE} -- best cross validated Mean Squared Error (n-fold) achieved during minimisation procedure.
#' @templateVar topLevel3 \item \strong{nFold} -- the input \code{nFold} parameter.
#' @templateVar topLevel4 \item \strong{gapCV} -- the input \code{gapCV} parameter.
#' @templateVar topLevel5 \item \strong{method} -- always contains string \code{"AutoSTR"} for this function.
#' @template returnValue
#' @examples
#' library(doParallel)
#' registerDoParallel(2)
#' plot(AutoSTR(grocery))
#' @author Alexander Dokumentov
#' @export

AutoSTR.ts = function(data, gapCV = NULL,
  lambdas = NULL, reltol = 0.001, confidence = NULL, nsKnots = NULL,
  trace = FALSE)
{
  if(!("ts" %in% class(data)))
    stop('Parameter "data" must be of class "ts".')
  # AutoSTR.msts also works with ts class
  str = AutoSTR.msts(data = data,
                     gapCV = gapCV,
                     lambdas = lambdas,
                     reltol = reltol,
                     confidence = confidence,
                     nsKnots = nsKnots,
                     trace = trace)
  return(str)
}
