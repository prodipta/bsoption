#' Option chain for July 2017 options on NSE (India) NIFTY 50 Index.
#'
#' A dataset containing the prices and other attributes of almost 8 options (
#' four calls and four puts) of NIFTY 50 Index for calibration of volatility.
#'
#' @format A data frame with 8 observations and 5 variables:
#' \describe{
#'   \item{strike}{strike of the option, in index points}
#'   \item{price}{price of the options, in INR}
#'   \item{forward}{forward level of the index}
#'   \item{expiry}{expiry date of the options}
#'   \item{type}{type of options, either call or put}
#' }
#' @source \url{https://www.nseindia.com/}
"opt_chain"
