#' @return A structure containing input and output data.
#' It is an \strong{S3} class \code{<%= class %>}, which is a list with the following components:
#' \itemize{
#' \item \strong{output} -- contains decomposed data. It is a list of three components:
#' \itemize{
#' \item \strong{predictors} -- a list of components where each component
#' corresponds to the input predictor. Every such component is a list containing the following:
#' \itemize{
#' \item \strong{data} -- fit/forecast for the corresponding predictor (trend, seasonal component, flexible or seasonal predictor).
#' \item \strong{beta} -- beta coefficients of the fit of the coresponding predictor.
#' \item \strong{lower} -- optional (if requested) matrix of lower bounds of confidence intervals.
#' \item \strong{upper} -- optional (if requested) matrix of upper bounds of confidence intervals.
#' }
#' \item \strong{random} -- a list with one component \strong{data}, which contains residuals of the model fit.
#' \item \strong{forecast} -- a list with two components:
#' \itemize{
#' \item \strong{data} -- fit/forecast for the model.
#' \item \strong{beta} -- beta coefficients of the fit.
#' \item \strong{lower} -- optional (if requested) matrix of lower bounds of confidence intervals.
#' \item \strong{upper} -- optional (if requested) matrix of upper bounds of confidence intervals.
#' }
#' }
#' \item \strong{input} -- input parameters and lambdas used for final calculations.
#' \itemize{
#' \item \strong{data} -- input data.
#' \item \strong{predictors} - input predictors.
#' \item \strong{lambdas} -- smoothing parameters used for final calculations (same as input lambdas for STR method).
#' }
#' <%= topLevel1 %>
#' <%= topLevel2 %>
#' <%= topLevel3 %>
#' <%= topLevel4 %>
#' <%= topLevel5 %>
#' }
