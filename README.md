# bsoption

The goal of this R package (bsoption) is to provide a quick platform for pricing index (and currency) exchange traded options. This also include a SABR and quadratic smile volatility calibration method. Also user can extract implied and realized distributions and use a given underlying distribution to price options for relative value analysis.

## Installation

You can install bsoption from github with:

```R
# install.packages("devtools")
devtools::install_github("prodipta/bsoption")
```

## Example

This is a basic example which shows you how to calibrate a SABR vol from market prices of options. The dataframe opt_chain is provided with the package:

```R
?opt_chain
sabrVol <- calibrate(opt_chain,Sys.Date(),model="sabr")
quadVol <- calibrate(opt_chain,Sys.Date())
k <- seq(90,110,0.5)
vols1 <- getVol(sabrVol,100,k)
vols2 <- getVol(quadVol,100,k)
plot(k,vols1,type="n")
lines(k,vols1,col="red")
lines(k,vols2,col="blue")
```
