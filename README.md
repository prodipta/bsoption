# bsoption
Package for option pricing and volatility calibration for index (and FX) options
The functions bsImpliedVol and bsPlainVanillaOption allows calculating implied volatility from market prices and pricing of vanilla options
The function calibrate fit a volatility model to market prices of options. The model can be either SABR or a simple quadratic smile model
The functions impliedDistribution and realizedDistribution allows extracting the implied (from a volatility object obtained through 
calibration) or realized (from time series of underlying prices) distribution of the underlying.
The function distributionPricer use a distribution (obtained through impliedDistribution or realizedDistribution) to price a given option
from the first principle.
