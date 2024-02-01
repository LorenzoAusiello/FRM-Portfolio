
#download the closing prices for heating oil futures 
library(quantmod)
oil.futures <- getSymbols('HO=F',auto.assign = F, 
                          from = as.Date("2000-09-01"), to = as.Date("2023-10-31"))
#download data for jet fuel prices
jet.fuel <- getSymbols('DJFUELUSGULF', src = 'FRED', auto.assign = F, 
                       from = as.Date("2000-09-01"), to = as.Date("2023-10-31"))

#Merge the two time series altogether and compute the return for each
oil.futures <- na.omit(oil.futures)
jet.fuel <- na.omit(jet.fuel)

#plot the cumulative return of both time series in a single plot.
dataset <- merge(jet.fuel, oil.futures$`HO=F.Adjusted`, join = "inner")

returns<- na.omit(log(dataset/lag(dataset)))

{plot(cumsum(returns$DJFUELUSGULF), type = "l", col = "blue",
      xlab="Date",ylab="Cumulative returns", main = "jet fuel (blue) vs heating oil futures (red)")
lines(cumsum(returns$HO.F.Adjusted), col = "red")}

#annual volatility for each. Comparison with that of the SPY over the same period
jf.sigma <- sd(returns$DJFUELUSGULF)*sqrt(252)
jf.sigma

oilf.sigma <- sd(returns$HO.F.Adjusted)*sqrt(252)
oilf.sigma 

SPY <- getSymbols('SPY',from = as.Date("2000-09-01"), to = as.Date("2023-10-31"), auto.assign = F)
SPY <- na.omit(log(SPY$SPY.Adjusted/lag(SPY$SPY.Adjusted)))
spy.sigma <- sd(SPY)*sqrt(252)
spy.sigma


#Regress the return on the jet fuel using the return on the futures contract
model <- lm(DJFUELUSGULF~HO.F.Adjusted, data = returns)
summary(model)              
model$coefficients[2]            

#hedged pfolio and sd for different N
N <- seq(0,2,0.001)
pfolio.sigma <-c()
for (n in N){
  pfolio <- returns$DJFUELUSGULF - n*returns$HO.F.Adjusted   
  pfolio.sigma <-c(pfolio.sigma, sd(pfolio)*sqrt(252))}

min.sigma <- min(pfolio.sigma)
min.N <- N[which.min(pfolio.sigma)]
{plot(N, pfolio.sigma, type = 'l', pch = 19)              
points(min.N, min.sigma, col="blue", cex = 1.5)
text(min.N, min.sigma, paste("Min Volatility:", round(min.sigma, 4)), pos = 4, cex = 0.8)}

#compare with beta
N[which.min(pfolio.sigma)]  
model$coefficients[2]   
