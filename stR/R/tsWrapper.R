#' @rdname AutoSTR.msts
#' @examples
#' # Decomposition of a monthly time series
#' decomp <- AutoSTR(grocery)
#' plot(decomp)
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
