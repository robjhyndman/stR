#' Common argument
#' @keywords internal
#' @name confidence
#' @param confidence A vector of percentiles giving the coverage of confidence intervals.
#' It must be greater than 0 and less than 1.
#' If \code{NULL}, no confidence intervals are produced.
NULL

#' Common argument
#' @keywords internal
#' @name data
#' @param data Time series or a vector of length \emph{\strong{L}}.
NULL

#' Common argument
#' @keywords internal
#' @name trace
#' @param trace When \code{TRUE}, tracing is turned on.
NULL

#' Common argument
#' @keywords internal
#' @name predictors
#' @param predictors List of predictors.\cr
#' According to the paradigm of this implementation, the trend, the seasonal components,
#' the flexible predictors and the seasonal predictors are all presented in the same
#' form (as predictors) and must be described in this list.\cr
#' Every predictor is a list of the following structures:\cr
#' \itemize{
#' \item \strong{data} -- vector of length \emph{\strong{L}} (length of input data,
#'   see above). For trend or for a seasonal component it is a vector of ones.
#'   For a flexible or a seasonal predictor it is a vector of the predictor's data.
#' \item \strong{times} -- vector of length \emph{\strong{L}} of times of observations.
#' \item \strong{seasons} -- vector of length \emph{\strong{L}}. It is a vector of ones
#'   for a trend or a flexible predictor. It is vector assigning seasons to every
#'   observation (for a seasonal component or a seasonal predictor).
#'   Seasons can be fractional for observations in between seasons.
#' \item \strong{timeKnots} -- vector of times (time knots) where knots are positioned
#'   (for a seasonal component or a seasonal predictor a few knots have the same time;
#'   every knot is represented by time and season). Usually this vector coincides with
#'   \strong{times} vector described above, or \strong{timeKnots} is a subset of
#'   \strong{times} vector.
#' \item \strong{seasonalStructure} -- describes seasonal topology (which can have complex
#'   structure) and seasonal knots.The seasonal topology is described by a list of
#'   segments and seasonal knots, which are positioned inside the segments, on borders of
#'   the segments or, when they are on on borders, they can connect two or more segments.\cr
#' \strong{seasonalStructure} is a list of two elements:\cr
#' \itemize{
#' \item \strong{segments} -- a list of vectors representing segments.
#' Each vector must contain two ordered real values which represent left and right borders
#'   of a segment. Segments should not intersect (inside same predictor).
#' \item \strong{sKnots} -- a list of real values (vectors of length one) or vectors of
#'   lengths two or greater (seasonal knots) defining seasons of the knots (every knot
#'   is represented by time and season). All real values must belong (be inside or on
#'   border of) segments listed in \strong{segments}. If a few values represent a single
#'   seasonal knot then all these values must be on borders of some segments (or a single
#'   segment). In this case they represent a seasonal knot which connects a few segments
#'   (or both sides of one segment).
#' }
#' \item \strong{lambdas} -- a vector with three values representing lambda (smoothing)
#'   parameters (time-time, season-season, time-season flexibility parameters) for this
#'   predictor.
#' }
NULL

#' Common argument
#' @keywords internal
#' @name strDesign
#' @param strDesign An optional parameter used to create the design
#' matrix. It is used internally in the library to improve performance when the design
#' matrix does not require full recalculation.
NULL

#' Common argument
#' @keywords internal
#' @name lambdas
#' @param lambdas An optional parameter.
#' A structure which replaces lambda parameters provided with predictors.
#' It is used as either a starting point for the optimisation of parameters or as the
#' exact model parameters.
NULL

#' Common argument
#' @keywords internal
#' @name reportDimensionsOnly
#' @param reportDimensionsOnly A boolean parameter. When TRUE the method constructs the
#' design matrix and reports its dimensions without proceeding further.
#' It is mostly used for debugging.
NULL

#' Common argument
#' @keywords internal
#' @name solver
#' @param solver A vector with two string values. The only supported combinations are:
#' c("Matrix", "cholesky") (default), and c("Matrix", "qr").
#' The parameter is used to specify a particular library
#' and method to solve the minimisation problem during STR decompositon.
NULL

#' Common argument
#' @keywords internal
#' @name nMCIter
#' @param nMCIter Number of Monte Carlo iterations used to estimate confidence intervals for Robust STR decomposition.
NULL

#' Common argument
#' @keywords internal
#' @name control
#' @param control Passed directly to \code{\link[quantreg]{rq.fit.sfn}()} during Robust STR decomposition.
NULL

#' Common argument
#' @keywords internal
#' @name pattern
#' @param pattern An optional parameter which has the same structure as \code{lambdas}
#' although with a different meaning. All zero values correspond to lambda
#' (smoothing) parameters which will not be estimated.
NULL

#' Common argument
#' @keywords internal
#' @name nFold
#' @param nFold An optional parameter setting the number of folds for cross validation.
NULL

#' Common argument
#' @keywords internal
#' @name reltol
#' @param reltol An optional parameter which is passed directly to \code{\link{optim}()}
#' when optimising the parameters of the model.
NULL

#' Common argument
#' @keywords internal
#' @name gapCV
#' @param gapCV An optional parameter defining the length of the sequence of
#' skipped values in the cross validation procedure.
NULL

#' Common argument
#' @keywords internal
#' @name robust
#' @param robust When \code{TRUE}, Robust STR decomposition is used. Default is \code{FALSE}.
NULL
