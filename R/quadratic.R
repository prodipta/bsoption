
calibrate.quadvol <- function(options, valuedate, precision=2, type="delta"){

  if(NROW(options)<3)
    stop("Too few inputs, need minimum 3 points to calibrate.")

  if(type == "delta"){
    options$k <- options$strike/ options$forward - 1
    options$k <- round(options$k, (2+precision))
  } else if(type == "strike"){
    options$k <- options$strike - options$forward
  } else{
    stop("Unknown volatility type: should be either delta or strike.")
  }

  reg_data <- data.frame(y=options$IV, x=options$k, xsq=(options$k)^2)
  fit <- stats::lm(y~.,data=reg_data); fit <- summary(fit)
  if(fit$r.squared < 0.9) warning("Goodness of fit is below threshold.")
  atm <- mean(options$IV); skew <- convexity <- 0
  if(abs(fit$coefficients[1,3])>2)atm <- fit$coefficients[1,1]
  if(abs(fit$coefficients[2,3])>2)skew <- fit$coefficients[2,1]
  if(abs(fit$coefficients[3,3])>2)convexity <- fit$coefficients[3,1]

  discfact <- 1
  if(!is.na(match("discount",colnames(options))))discfact<-options$discount[1]
  vol <- list(valuedate=valuedate,atmvol=atm,skew=skew,convexity=convexity,
              t=options$t[1], discfact= discfact, type=type)
  class(vol) <- "quadvol"
  fitted_vols <- bsoption::getVol(vol,options$forward,options$strike)
  err <- (fitted_vols - options$IV)^2; err <- sqrt(sum(err))
  if(err/NROW(options) > 1e-03){
    rms <- round(err/NROW(options),6)
    warning(paste("Average error:",rms,". Goodness of fit is below threshold."))
  }

  return(vol)
}

#'@export
getVol.quadvol <- function(vol,atm, strike){
  ret <- vol$atm
  if(vol$type=="delta"){
    k <- strike/atm -1
  } else if(vol$type=="strike"){
    k <- strike - atm
  } else {
    stop("Unknown volatility type: should be either delta or strike")
  }

  ret <- vol$atm + k*vol$skew + (k^2)*vol$convexity
  return(ret)
}
#'@export
bumpVol.quadvol <- function(vol,bump=0.01){
  vol$atmvol <- vol$atmvol + bump
  return(vol)
}
