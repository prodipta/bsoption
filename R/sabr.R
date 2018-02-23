
sabrVol <- function(f,k,t,a,b,r,v){
  if(abs(f/k-1) < 1e-04){
    term1 <- a/(f^(1-b))
    term2 <- ((((1-b)^2)/24)*((a^2)/f^(2-2*b)) + r*b*a*v/(4*f^(1-b))
              + (2-3*r^2)*v^2/24)
    y <- term1*(1 + term2*t)

  } else{
    fk <- f*k
    z <- v/a*(fk)^((1-b)/2)*log(f/k)
    x <- suppressWarnings(log((sqrt(1-2*r*z + z^2) + z-r)/(1-r)))
    term1 <- a / fk^((1-b)/2) / (1 + (1-b)^2/24*log(f/k)^2 +
                                   (1-b)^4/1920*log(f/k)^4)
    if(is.nan(x)){
      term2 <- 1
    } else{
      term2 <- z / x
    }
    term3 <- 1 + ((1-b)^2/24*a^2/fk^(1-b) +
                    r*b*v*a/(4*fk^((1-b)/2)) + (2-3*r^2)/24*v^2)*t
    y <- term1*term2*term3
  }
  return(y)
}
findAlpha <- function(f,t,atmvol,b,r,v){
  # complete solution based on West (2005)
  #c0 <- -atmvol*(f^(1-b))
  #c1 <- (1+((2-3*(r^2))*(v^2)*t/24))
  #c2 <- (r*b*v*t)/(4*f^(1-b))
  #c3 <- (((1-b)^2)*t)/(24*f^(2-2*b))
  #roots <- polyroot(c(c0,c1,c2,c3))
  # remove any complext or negative roots
  #roots <- Re(roots[which(Im(roots)==0 & Re(roots)>0)])
  #if(length(roots)==0) roots <- 0
  # pick the minimum value
  #return(min(roots))
  # apporximation from Hagan (2003) 3.1a, but very fast and stable
  a <- atmvol*(f^(1-b))
  return(a)
}
squared_error <- function(params,strikes,IV,atmvol,fwd,t,b){
  N <- NROW(strikes)
  err <- rep(0,N)
  f <- fwd
  r <- params[1]; v <- params[2]
  a <- findAlpha(f,t,atmvol,b,r,v)
  for(i in 1:N){
    err[i] <- (sabrVol(f,strikes[i],t,a,b,r,v) - IV[i])^2
  }
  y <- sum(err)

  # penalities for non-admissible values of r and v
  if(abs(r)>1) y <- Inf
  if(v<0) y <- Inf

  return(y)
}
calibrate.sabrvol <- function(options, valuedate, atmvol, precision){
 if(atmvol==0){
   atmvol <- (calibrate.quadvol(options,valuedate,precision,
                                           "delta"))$atmvol
 }
 f <- options$forward[1]; t <- options$t[1]; b <- 0
 strikes <- options$strike; IV <- options$IV
 params <- c(-0.5,0.5)
 fit <- stats::optim(params,fn=squared_error, method = "SANN",
       strikes=strikes,IV=IV,atmvol=atmvol,f=f,t=t,b=b)

 if(sqrt(fit$value)/NROW(strikes) > 1e-03){
   rms <- round(sqrt(fit$value)/NROW(strikes),6)
   warning(paste("Average error:",rms,". Goodness of fit is below threshold."))
 }

 r = fit$par[1]; v=fit$par[2]
 discfact <- 1
 if(!is.na(match("discount",colnames(options))))discfact<-options$discount[1]
 vol <- list(valuedate=valuedate, atmvol=atmvol,b=b,r=r,v=v,t=t,discfact=discfact)
 class(vol) <- "sabrvol"
 return(vol)
}

#'@export
getVol.sabrvol <- function(vol,atm, strike){
  N <- max(NROW(atm),NROW(strike))
  strike <- utils::head(rep(strike,N),N)
  atm <- utils::head(rep(atm,N),N)
  vols <- rep(0,N)

  for(i in 1:N){
    a <- findAlpha(atm[i],vol$t,vol$atmvol,vol$b,vol$r,vol$v)
    vols[i] <- sabrVol(atm[i],strike[i],vol$t,a,vol$b,vol$r,vol$v)
  }

  return(vols)
}
#'@export
bumpVol.sabrvol <- function(vol,bump=0.01){
  vol$atmvol <- vol$atmvol + bump
  return(vol)
}
