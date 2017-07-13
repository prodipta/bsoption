

bsOptionPrice <- function(forward, strike, time, vol,
                          type="call", discount=1, model='lognormal'){
  if (model != 'lognormal')
    stop('not implemented yet')

  d1 <- (log(forward/strike) + 0.5*time*vol^2)/(vol*sqrt(time))
  d2 <- (log(forward/strike) - 0.5*time*vol^2)/(vol*sqrt(time))

  if (type=="call"){
    price <- discount*(forward*stats::pnorm(d1) - strike*stats::pnorm(d2))
  } else{
    price <- discount*(strike*stats::pnorm(-d2) - forward*stats::pnorm(-d1))
  }
  return(price)
}
bsOptionsGreek <- function(forward, strike, time, vol, greek="delta",
                           type="call", discount=1, model='lognormal'){
  if (model != 'lognormal')
    stop('not implemented yet')
  t <- time
  price <- bsOptionPrice(forward, strike, t, vol, type, discount, model)

  if(greek=="price"){
    return(price)
  } else if(greek=="delta"){
    price.up <- bsOptionPrice(forward*1.0025, strike, t, vol, type, discount, model)
    price.dn <- bsOptionPrice(forward*0.9975, strike, t, vol, type, discount, model)
    delta.up <- price.up - price
    delta.dn <- price - price.dn
    delta.ave <- 0.5*(delta.up + delta.dn)/(0.0025*forward)
    return(delta.ave)
  } else if(greek=="vega"){
    price.up <- bsOptionPrice(forward, strike, t, vol+0.01, type, discount, model)
    price.dn <- bsOptionPrice(forward, strike, t, vol-0.01, type, discount, model)
    vega.up <- price.up - price
    vega.dn <- price - price.dn
    vega.ave <- 0.5*(vega.up + vega.dn)
    return(vega.ave)
  } else if(greek=="theta"){
    price.up <- bsOptionPrice(forward, strike, t-1/260, vol, type, discount, model)
    theta <- price.up - price
    return(theta)
  } else if(greek=="gamma"){
    delta <- bsOptionsGreek(forward, strike, t, vol,"delta", type, discount, model)
    delta.up <- bsOptionsGreek(forward*1.01, strike, t, vol,"delta", type, discount, model)
    delta.dn <- bsOptionsGreek(forward*0.99, strike, t, vol, "delta" , type, discount, model)
    gamma.up <- delta.up - delta
    gamma.dn <- delta - delta.dn
    gamma.ave <- 0.5*(gamma.up + gamma.dn)
    return(gamma.ave)
  } else{
    stop("not implemented yet")
  }
}
bsVolSolve <- function(forward, strike, time, price,
                       type="call", discount=1, model='lognormal',
                       tolerance = 0.00001, iteration=100){
  if (model != 'lognormal')
    stop('not implemented yet')

  #guess <- price/(0.4*forward*sqrt(time))
  guess <- 0.5
  i=1; vol = guess; h=tolerance
  while(i<iteration){
    px <- bsOptionPrice(forward, strike, time, vol,
                        type, discount, model) - price
    px.dv <- bsOptionPrice(forward, strike, time, vol+ h,
                           type, discount, model) - price
    dpx.dvol <- (px.dv - px)/(h)
    vol.1 <- vol - px/dpx.dvol
    i <- i+1
    if(abs(vol.1 - vol) < tolerance) break
    vol <- vol.1
  }

  return(vol)
}

#'Compute price and greeks of plain vanilla European options
#'@description Compute price and greeks of plain vanilla European options (
#'calls and puts) using lognormal Black-76 model.
#'@param date Value date of the option.
#'@param forward Forward level(s) of the underlying.
#'@param strike strike(s) of the option.
#'@param expiry Expiry date(s).
#'@param vol Volatility(s).
#'@param type Option type - "call" or "put".
#'@param discount Discount factor.
#'@param output Output type, "price" or any other standard greeks.
#'@param model Model type, only lognormal is implemented.
#'@return Price or greek estimate. The function is vectorized.
#'@details Black's formula is arguably more useful for pricing index options
#'compared to traditional Black-Scholes formula. The Black-76 option pricing
#'model uses the price of futures directly, instead of accreting the spot
#'index at the cost of carry. This is useful where the index futures trade
#'at prices below spot plus cost of carry, providing a model consistent with
#'market. In many places this is now standard to use Black-76 for index
#'options valuation for margining and fair value determination purposes. For
#'greeks the return values are as follows: 1) delta: this returns the
#'standard black-scholes delta, the first derivative of option price with
#'respect to underlying. 2) gamma: this returns the change in delta for a
#'1 percentage point change in underlying (averaged for up and down move) 3)
#'vega: this returns the change in option price for a absolute 1 percentage
#'points increase in volatility 4) theta: this returns the decay in option
#'price over one day (assumeing 260 business days in a year).
#'@export
bsPlainVanillaOption <- function(date, forward, strike, expiry, vol,
                                 type="call", discount=1, output='price', model='lognormal'){
  type <- tolower(type)
  output <- tolower(output)
  if (model != 'lognormal')stop('not implemented yet')

  N <- max(NROW(forward),NROW(strike),NROW(expiry),NROW(vol),NROW(type),
           NROW(discount))
  forward <- utils::head(rep(forward,N),N)
  strike <- utils::head(rep(strike,N),N)
  expiry <- utils::head(rep(expiry,N),N)
  vol <- utils::head(rep(vol,N),N)
  type <- utils::head(rep(type,N),N)
  discount <- utils::head(rep(discount,N),N)
  price <- rep(0,N)

  suppressWarnings(if(class(expiry)=="Date")expiry <- as.POSIXct.Date(expiry))
  suppressWarnings(if(class(date)=="Date")date <- as.POSIXct.Date(date))

  t = (as.numeric(expiry) - as.numeric(date))/(60*60*24*260)

  for(i in 1:N){
    price[i] <- bsOptionsGreek(forward[i], strike[i], t[i], vol[i], output,
                               type[i], discount[i], model)
  }
  return(price)
}

#'Compute implied volatility of plain vanilla European options from market
#'price
#'@description Compute implied volatility of plain vanilla European options
#'from market price using lognormal Black-76 model.
#'@param date Value date of the option.
#'@param forward Forward level(s) of the underlying.
#'@param strike strike(s) of the option.
#'@param expiry Expiry date(s).
#'@param price Option market price(s).
#'@param type Option type - "call" or "put".
#'@param discount Discount factor.
#'@param model Model type, only lognormal is implemented.
#'@return Implied volatility estimate. The function is vectorized.
#'@details Black's formula is arguably more useful for pricing index options
#'compared to traditional Black-Scholes formula. The Black-76 option pricing
#'model uses the price of futures directly, instead of accreting the spot
#'index at the cost of carry. This is useful where the index futures trade
#'at prices below spot plus cost of carry, providing a model consistent with
#'market. In many places this is now standard to use Black-76 for index
#'options valuation for margining and fair value determination purposes.
#'@export
bsImpliedVol <- function(date, forward, strike, expiry, price,
                         type="call", discount=1, model='lognormal'){
  type <- tolower(type)
  if (model != 'lognormal')
    stop('not implemented yet')

  N <- max(NROW(forward),NROW(strike),NROW(expiry),NROW(price),NROW(type),
           NROW(discount))
  forward <- utils::head(rep(forward,N),N)
  strike <- utils::head(rep(strike,N),N)
  expiry <- utils::head(rep(expiry,N),N)
  price <- utils::head(rep(price,N),N)
  type <- utils::head(rep(type,N),N)
  discount <- utils::head(rep(discount,N),N)
  vol <- rep(0,N)

  suppressWarnings(if(class(expiry)=="Date")expiry <- as.POSIXct.Date(expiry))
  suppressWarnings(if(class(date)=="Date")date <- as.POSIXct.Date(date))

  t = (as.numeric(expiry) - as.numeric(date))/(60*60*24*260)

  for(i in 1:N){
    vol[i] <- bsVolSolve(forward[i], strike[i], t[i], price[i], type[i])
  }

  return(vol)
}
