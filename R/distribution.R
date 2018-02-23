
#'Extract implied distribution of the underlying from options prices
#'@description Extract implied probability distribution from option prices
#'from a fitted vol model calibrated to market prices.
#'@param vol A vol object (either sabrvol or quadvol) obtained by calibrating
#'to market prices of options.
#'@param n Number of division for the range of strikes to generate the
#'distribution.
#'@param bounds The range (in percentage) of the underlying to estimate the
#'distribution.
#'@param normalization A logical value. If true, the implied distribution is
#'normalized to sum up to 1, else not.
#'@return A dataframe with strikes and corresponding probability density
#'function estimates.
#'@details Extracting probability density from the implied volatility of
#'options is done using the standard method of taking the second derivatives
#'of option prices with respect to strikes. The second derivative is estimated
#'using the usual finite difference approach. The resulting distribution is
#'the option implied risk neutral distribution. The strikes of the underlying
#'of the distribution is always scaled so that the current forward is at 100.
#'@seealso \code{\link{calibrate}} for calibration and volatility models
#'@export
impliedDistribution <- function(vol, n=512, bounds=c(-0.2, 0.2),
                                normalization=TRUE){
  lowstrike <- 100*(1+bounds[1])
  highstrike <- 100*(1+bounds[2])
  strikes <- seq(lowstrike,highstrike,length.out = n)
  px <- rep(0,n)
  d <- abs(strikes[2] - strikes[1]); h <- 0.025*d

  for(i in 1:n){
    k <- strikes[i]
    x1 <- bsOptionPrice(100,k+h,vol$t,getVol(vol,100,k+h),"call",vol$discfact)
    x2 <- bsOptionPrice(100,k,vol$t,getVol(vol,100,k),"call",vol$discfact)
    x3 <- bsOptionPrice(100,k-h,vol$t,getVol(vol,100,k-h),"call",vol$discfact)
    px[i] <- (x1 - 2*x2 + x3)/h^2
  }

  #0-th momemnt normalization to sum up to 1
  if(normalization){
    px <- px/sum(px*d)
  }

  p <- data.frame(k=strikes,p=px)
  return(p)
}

#'Extract realized distribution of the underlying prices
#'@description Extract realized probability distribution from the time series
#'data of the underlying price.
#'@param x Time series of underlying prices in xts format. The frequency is
#'assumed to be daily.
#'@param t Maturity (in years). This is used to scale the daily returns using
#'a square root of time scaling.
#'@param n Number of division for the range of strikes to generate the
#'distribution. This number should ideally be a power of 2 (becuase of Fourier
#'transform used within) although it is not a requirement.
#'@param bounds The range (in percentage) of the underlying to estimate the
#'distribution.
#'@param normalization A logical value. If true, the implied distribution is
#'normalized to sum up to 1, as well as the first moment is normalized to
#'be equal to the forward level (100).
#'@return A dataframe with strikes and corresponding probability density
#'function estimates.
#'@details Extracting probability density from the prices data is done in two
#'steps. First the prices are converted to returns and appropriately scaled.
#'Then these scaled returns are use to fit a  (gaussian) kernel density
#'estimate. The probability distribution is normalized in the zero-th
#'moment to sum up to 1 and also in the first moment to match the forward
#'(100). The strikes of the underlying of the distribution is always scaled
#'so that the current forward is at 100.
#'@export
realizedDistribution <- function(x,t,n=512,bounds=c(-0.2, 0.2),
                                 normalization=TRUE){
  lowstrike <- 100*(1+bounds[1])
  highstrike <- 100*(1+bounds[2])
  strikes <- seq(lowstrike,highstrike,length.out = n)

  x <- stats::na.omit(TTR::ROC(quantmod::Cl(x)))
  dt <- t/(1/260)
  x <- x*sqrt(dt)
  y <- stats::density(x, n=n, from = bounds[1], to = bounds[2])
  k <- 100*(1+y$x); px=y$y
  d <- abs(k[2] - k[1])

  #0-th momemnt normalization to sum up to 1
  if(normalization)
    px <- px/sum(px*d)

  #1st moment normalization to forward level 100
  if(normalization){
    offset <- 100 - sum(k*px)*d
    k <- k + offset
  }

  p <- data.frame(k=k,p=px)
  return(p)
}

#'Price vanilla European options from a given underlying distribution
#'@description Price vanilla European call or put options from a given
#'underlying distribution, using the first principle.
#'@param forward Forward level of the underlying.
#'@param strike Strike of the option.
#'@param voldist The distribution of the underlying (obtained using either
#'impliedDistribution or realizedDistribution function).
#'@param type The type of the option (call or put).
#'@param discount Discount factor.
#'@param model Model type, only lognormal is implemented.
#'@return Price of the option.
#'@details The underlying distribution is used to compute the payoff profile
#'of option and this is then integrated within the range to arrive at the
#'price of the option. All values are assumed to be zero outside the specified
#'distribution range.
#'#'@seealso \code{\link{impliedDistribution}}
#'@export
distributionPricer <- function(forward, strike, voldist,
                               type="call", discount=1, model='lognormal'){
  type <- tolower(type)
  f <- function(x, voldist, k){
    payoff <- voldist$p*(voldist$k-k)
    payoff <- ifelse(payoff<0,0,payoff)
    y <- stats::approx(voldist$k,payoff,x,n=NROW(x),rule=2,yleft = 0,yright = 0)$y
    return(y)
  }
  k <- 100*strike/forward
  payoff <- stats::integrate(f,k,Inf,voldist=voldist,k=k,subdivisions = 500)$value
  if(type=="call"){
    px <- payoff
  } else if(type=="put"){
    px <- payoff - (100-k)
  } else{
    stop("Unknown option type. Can be either call or put")
  }
  px <- discount*px*forward/100
  return(px)
}
