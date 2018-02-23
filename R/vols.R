
#'Calibrate vols (SABR or quadratic smile fit) from market prices
#'@description The first input is option chain data (dataframe) for a single
#'maturity, the expected columns include the forward, expiry (in date format),
#'strike, price and type (either call or put).
#'@param options dataframe with options chain details
#'@param valuedate valuation date for the calibration
#'@param model Volatility model, can be either sabr or quadratic
#'@param atmvol ATM volatility, if provided used in sabr calibration
#'(else ATM vol is estimated with a quadratic vol fit first)
#'@param precision degree of precision
#'@param type Type of fit in case of quadratic model (either delta or strike)
#'@return Fitted volatility object. The object is either of class quadvol or
#'sabrvol depending upon the fit. The object includes atm vol level, and model
#'specific parameters. It also stores the valuation date of calibration. The
#'sabr model implements calibration of a lognormal SABR model (ref Hagan(2002)
#'"Managing smile risk" and West (2005) "Calibration of the SABR Model in
#'Illiquid Markets"). The model assumes a beta of 0 (lognormal behaviour) and
#'calibrates other parameters of SABR. The quadratic model is a simple two
#'parameters model of volatility smile, fitted with least square regression.
#'The calibration type delta fits this quadratic smile based on percentage
#'offset from ATM, while strike fits absolute deviation from ATM forward.
#'@export
calibrate <- function(options, valuedate, model="quadratic",atmvol=0,
                      precision=2, type="delta"){
  cols <- tolower(colnames(options))
  expected_cols <- c("forward","expiry","strike","price","type")
  idx <- unlist(apply(as.matrix(expected_cols),1,FUN = function(x){
    if(length(grep(x,cols))>0)return(grep(x,cols))
    return(0)
  }))

  if(any(idx==0))
    stop("Input option prices are not in correct format. See help.")
  colnames(options)[idx] <- expected_cols

  if(NROW(unique(options$forward)) !=1 | NROW(unique(options$forward)) !=1)
    stop("Multiple expiries found. Calibrate one expiry series at a time.")

  options$type <- tolower(options$type)
  options$price[options$forward > options$strike & options$type=="call"] <- NA
  options$price[options$forward < options$strike & options$type=="put"] <- NA
  options <- stats::na.omit(options)

  if(NROW(unique(options)) != NROW(options))
    stop("Duplicate strikes found. Remove duplicate strikes.")

  discfact <- 1
  if(!is.na(match("discount",colnames(options))))discfact<-options$discount[1]

  options$IV <- bsImpliedVol(valuedate,options$forward,options$strike,
                             options$expiry,options$price,options$type,discfact)
  options$IV <- round(options$IV,(2+precision))

  suppressWarnings(if(class(options$expiry)=="Date")
    options$expiry <- as.POSIXct.Date(options$expiry))
  suppressWarnings(if(class(valuedate)=="Date")
    valuedate <- as.POSIXct.Date(valuedate))

  options$t <- (as.numeric(options$expiry) - as.numeric(valuedate))
  options$t <- options$t/(24*60*60*260)

  if(model=="quadratic"){
    vol <- calibrate.quadvol(options, valuedate, precision, type)
  } else if(model=="sabr"){
    vol <- calibrate.sabrvol(options, valuedate, atmvol, precision)
  } else{
    stop("Unknown volatility model: only sabr and quadratic implemented.")
  }
  return(vol)
}

#'get volatility at a given strike
#'@description Returns the volatility at a given strike. This function is
#'vectorized.
#'@param vol Object returned from earlier calibration. Either a quadvol or a
#'sabrvol object.
#'@param atm ATM forward(s)
#'@param strike Options strike(s)
#'@return Black volatility at the given strike
#'@seealso \code{\link{calibrate}} for calibration and models
#'@export
getVol <- function(vol, atm, strike){
  UseMethod("getVol",vol)
}

#'bump the level of volatility for scenario analysis
#'@description Returns the volatility with a bumped up ATM vol levels
#'@param vol Object returned from earlier calibration. Either a quadvol or a
#'sabrvol object.
#'@param bump Bump amount. This is added to the ATM vol level
#'@return A volatility object of class quadvol or sabrvol
#'@seealso \code{\link{calibrate}} for calibration and models
#'@export
bumpVol <- function(vol,bump=0.01){
  UseMethod("bumpVol",vol)
}
