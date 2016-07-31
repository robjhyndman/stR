#' Common argument
#' @keywords internal
#' @name confidence
#' @param confidence A vector of confidence percentiles. It must be gerater than 0 and less than 1.
NULL

#' Common argument
#' @keywords internal
#' @name data
#' @param data Time series or a vector of some length \emph{\strong{L}}.
NULL

#' Common argument
#' @keywords internal
#' @name predictors
#' @param predictors List of predictors.\cr
#' According to the paradigm of this implementaion, the trend, the seasonal components, the flexible predictors
#' and the seasonal predictors are all presented in the same form (as predictors) and must be described in this list.\cr
#' Every predictor is a list of the following structures:\cr
#' \itemize{
#' \item \strong{data} -- vector of length \emph{\strong{L}} (length of input data, see above). For trend or for a seasonal component it is a vector of ones.
#' For a flexible or a seasonal predictor it is a vector of the predictor's data.
#' \item \strong{times} -- vector of length \emph{\strong{L}} of times of observations.
#' \item \strong{seasons} -- vector of length \emph{\strong{L}}. It is a vector of ones for a trend or a flexible predictor.
#' It is vector assigning seasons to every obesrvation (for a seasonal component or a seasonal predictor).
#' Seasons can be fractional for observations in between seasons.
#' \item \strong{timeKnots} -- vector of times (time knots) where knots are positioned
#' (for a seasonal component or a seasonal predictor a few knots have the same time; every knot is represented by time and season).
#' Usually this verctor coinsides with \strong{times} vector described above, or \strong{timeKnots} is a subset of \strong{times} vector.
#' \item \strong{seasonalStructure} -- describes seasonal topology (which can have complex structure) and seasonal knots.
#' The seasonal topology is described by a list of segments and seasonal knots,
#' which are positioned inside the segments, on borders of the segments or, when they are on on boreders, they can connect two or more segments.\cr
#' \strong{seasonalStructure} is a list of two elements:\cr
#' \itemize{
#' \item \strong{segments} -- a list of vectors representing segments.
#' Each vector must contain two ordered real values which represent left and right borders of a segment.
#' Segments should not intersect (inside same predictor).
#' \item \strong{sKnots} -- a list of real values (vectors of length one) or vectors of lengths two or greater (seasonal knots) defining seasons of the knots (every knot is represented by time and season).
#' All real values must belong (be inside or on border of) segments listed in \strong{segments}.
#' If a few values represent a single seasonal knot then all these values must be on borders of some segments (or a single segment).
#' In this case they represent a seasonal knot which connects a few segments (or both sides of one segment).
#' }
#' \item \strong{lambdas} -- a vector with three values representing lambda (smoothing) parameters (time-time, season-season, time-season flexibility parameters) for this predictor.
#' }
NULL

#' Common argument
#' @keywords internal
#' @name strDesign
#' @param strDesign An optional parameter. A structure which is used to create the design matrix. It is used internally in the library to improve performance when the design matrix does not require full recalculation.
NULL

#' Common argument
#' @keywords internal
#' @name lambdas
#' @param lambdas An optional parameter. A structure which replaces lambda parameters provided with predictors (see /strong{lambdas} inside predictors parameter). It is used intrnally in the library.
NULL

#' Common argument
#' @keywords internal
#' @name reportDimensionsOnly
#' @param reportDimensionsOnly A boolean paramter. When TRUE the method constructs the design matrix and reports its dimentions without proceeding further.
#' It was mostly used for debugging.
NULL

#' Common argument
#' @keywords internal
#' @name solver
#' @param solver is "MatrixModels" or "cholesky". Used to specify a particlular library and method to solve the minimisation problem.
NULL

#' Common argument
#' @keywords internal
#' @name nMCIter
#' @param nMCIter Number of Monte Carlo iterations to estimate confidence intervals.
NULL

#' Common argument
#' @keywords internal
#' @name control
#' @param control Passed directly to \code{\link{rq.fit.sfn}} function.
NULL

#' Common argument
#' @keywords internal
#' @name lambdas
#' @param lambdas An optional parameter.
#' A structure which replaces lambda parameters provided with predictors.
#' It is used as a starting point for the model parameters optimisation.
NULL

#' Common argument
#' @keywords internal
#' @name pattern
#' @param pattern An optional parameter which has same structure as \code{lambdas} parameter although with a different meaning.
#' All zero values corespond to lambda (smoothing) parameters which will not be estimated.
NULL

#' Common argument
#' @keywords internal
#' @name nFold
#' @param nFold An optional parameter setting number of folds for cross validation.
NULL

#' Common argument
#' @keywords internal
#' @name reltol
#' @param reltol An optional parameter which is passed directly to \code{\link{optim}} R function. \code{\link{optim}} is used to optimise parameters of the model.
NULL

#' Common argument
#' @keywords internal
#' @name gapCV
#' @param gapCV An optional parameter to define how long should be the sequence of missed values in cross validation procedure.
NULL
