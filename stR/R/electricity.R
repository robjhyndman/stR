#' Electricity consumption dataset
#'
#' The data set provides information about electricity consumption in Victoria, Australia
#' during the 115 days starting on 10th of January, 2000,
#' and comprises the maximum electricity demand in Victoria during 30-minute periods
#' (48 observations per day). For each 30-minute period, the dataset also provides
#' the air temperature in Melbourne.
#'
#'\itemize{
#' \item \code{Consumption} column contains maximum electricity consumption during 30 minute periods
#' \item \code{Temperature} column contains temperature in Melbourne during the corresponding 30 minute interval
#' \item \code{Time} column contains number of 30 minute interval in the dataset
#' \item \code{DailySeasonality} column contains positions of 30 minute interval inside days
#' \item \code{WeeklySeasonality} column contains positions of 30 minute interval inside weeks
#' \item \code{WorkingDaySeasonality} column contains positions of 30 minute intervals
#' inside working day/holiday transition diagram
#'}
#'
#' @docType data
#'
#' @usage electricity
#'
#' @format An object of class \code{data.frame}.
#'
#' @keywords datasets
#'
#' @examples
#' plot(electricity$Demand, ylab = "Electricity demand", type = "l")
#' plot(electricity$TempMelb, ylab = "Temperature in Melbourne", type = "l")
#' plot(electricity$TempFr, ylab = "Temperature in Frankston", type = "l")
"electricity"
