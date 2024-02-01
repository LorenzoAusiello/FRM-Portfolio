
##Problem 1 

#Setting the directory
setwd("C:/Users/loaus/OneDrive - stevens.edu/STEVENS/Intro to Financial Risk Management/Project/Project2")
#setwd("C:/Users/lausiell/OneDrive - stevens.edu/STEVENS/Intro to Financial Risk Management/Project/Project2")

#Downloading data from csv file
tbond.data<-read.csv('bonds.csv')
head(tbond.data)

#Downloading Treasury Yields from FRED Database
library(quantmod)
getSymbols(c("DGS2", "DGS5", "DGS10", "DGS30"), src = 'FRED')

#Merging Treasury Yields and removing missing values
t.yields <- na.omit(merge(DGS2, DGS5, DGS10, DGS30))
t.yields <- t.yields["2018-01-02/2022-03-31"]

#Retrieving dates
tol <- 0.02
period1 <- t.yields[abs(t.yields$DGS2 - 2.88) <= tol & abs(t.yields$DGS5 - 3.07) <= tol
                    & abs(t.yields$DGS10 - 3.23) <= tol & abs(t.yields$DGS30 - 3.40) <= tol]
period1 

tol <- 0.025
period2 <- t.yields[abs(t.yields$DGS2 - 1.47) <= tol & abs(t.yields$DGS5 - 1.40) <= tol
                    & abs(t.yields$DGS10 - 1.56) <= tol & abs(t.yields$DGS30 - 2.06) <= tol]
period2 

tol <- 0.05
period3 <- t.yields[abs(t.yields$DGS2 - 0.31) <= tol & abs(t.yields$DGS5 - 0.46) <= tol
                    & abs(t.yields$DGS10 - 0.85) <= tol & abs(t.yields$DGS30 - 1.42) <= tol]
period3 

tol <- 0.015
period4 <- t.yields[abs(t.yields$DGS2 - 0.15) <= tol & abs(t.yields$DGS5 - 0.36) <= tol
                    & abs(t.yields$DGS10 - 0.82) <= tol & abs(t.yields$DGS30 - 1.60) <= tol]
period4[3] 

tol <- 0.015
period5 <- t.yields[abs(t.yields$DGS2 - 0.16) <= tol & abs(t.yields$DGS5 - 0.87) <= tol 
                    & abs(t.yields$DGS10 - 1.66) <= tol & abs(t.yields$DGS30 - 2.32) <= tol]
period5[1]

tol <- 0.05
period6 <- t.yields[abs(t.yields$DGS2 - 2.38) <= tol & abs(t.yields$DGS5 - 2.50) <= tol
                    & abs(t.yields$DGS10 - 2.39) <= tol & abs(t.yields$DGS30 - 2.49) <= tol]
period6 

#Calculating prices and comparing with the given ones
bondprice <- function(Coupon, Yield, Maturity, Face){
  Yield <- Yield/100
  Coupon <- Coupon/100
  Coupon/Yield*Face*(1-1/((1+Yield)^Maturity))+Face/((1+Yield)^Maturity)}

Face <- 100
tbond.data$Bond_Price <- apply(tbond.data, 1, function(row) {
  bondprice(row["Coupon"], row["Yield"], row["Maturity"], Face)
})


{plot(tbond.data$Price, tbond.data$Bond_Price, xlab="Bond Price Given", ylab = "Bond Price Computed")
  abline(0, 1, col = "blue")}

#Calculating yields and comparing with the given ones

bondyield <- function(yield, Coupon, Price, Maturity, Face){
  Coupon <- Coupon/100
  Bprice <- Coupon/yield*Face*(1-1/((1+yield)^Maturity))+Face/((1+yield)^Maturity) - Price
  return(Bprice)
}

tbond.data$Bond_Yield <- apply(tbond.data, 1, function(row) {
  Coupon <- row["Coupon"]
  Price <- row["Price"]
  Maturity <- row["Maturity"]
  Face <- 100
  uniroot(function(y) bondyield(y, Coupon, Price, Maturity, Face), c(0.0001, 1))$root
})*100

{plot(tbond.data$Yield, tbond.data$Bond_Yield, xlab = "Bond Yield Given", ylab = "Bond Yield Computed")
  abline(0, 1, col = "blue")}


#Compute Macaulay duration
mcduration <- function(Coupon, Bond_Price, Yield, Maturity){
  w <- c()
  for (i in 1:Maturity){
    w[i] <- Coupon/((1+Yield/100)^i)/Bond_Price} 
  w[Maturity] <- w[Maturity]+(100/((1+Yield/100)^Maturity))/Bond_Price
  Duration <- sum(w*(1:Maturity))
  return(Duration)}

tbond.data$Bond_Duration <- apply(tbond.data, 1, function(row) {
  mcduration(row["Coupon"], row["Bond_Price"],row["Yield"], row["Maturity"])
})

library(doBy)
summaryBy(Bond_Duration ~ Maturity, data = tbond.data, FUN = c(min,max))

#calculate the change in the Treasury bond prices Using first order Taylor expansion
newprices <- c()
for (i in 21:24){
  newprices[i-20] <- tbond.data$Bond_Price[i]-0.005*tbond.data$Bond_Duration[i]/(1+tbond.data$Yield[i]/100)*tbond.data$Bond_Price[i]}

{plot(tbond.data$Maturity[21:24], newprices, ylim = c(70,110), col = 'red',
      xlab = "Maturity", ylab = "Bond Price", main = "Prices before (blue) and after (red) interest rate increase")
points(tbond.data$Maturity[21:24], tbond.data$Bond_Price[21:24], col = 'blue')}

apply(tbond.data, 1, function(row) {
  bondprice(row["Coupon"], row["Yield"]+0.50, row["Maturity"], Face)})[21:24]==newprices

apply(tbond.data, 1, function(row) {
  bondprice(row["Coupon"], row["Yield"]+0.50, row["Maturity"], Face)})[21:24]-newprices

#Portfolio allocation
w21 <- (4-tbond.data$Bond_Duration[22])/(tbond.data$Bond_Duration[21]-tbond.data$Bond_Duration[22])
w22 <- (1-w21)
duration.tot <- w21*tbond.data$Bond_Duration[21] + w22*tbond.data$Bond_Duration[22]
n22 <- w22*100000/tbond.data$Price[22] 
n21<- w21*100000/tbond.data$Price[21] 
data.frame(Bond21=c(w21,n21), Bond22=c(w22,n22), row.names = c("weights", "units"))
duration.tot

w21 <- (8-tbond.data$Bond_Duration[22])/(tbond.data$Bond_Duration[21]-tbond.data$Bond_Duration[22])
w22 <- (1-w21)
duration.tot <- w21*tbond.data$Bond_Duration[21] + w22*tbond.data$Bond_Duration[22]
n22 <- w22*100000/tbond.data$Price[22] 
n21<- w21*100000/tbond.data$Price[21] 
data.frame(Bond21=c(w21,n21), Bond22=c(w22,n22), row.names = c("weights", "units"))
duration.tot

# Bonus question
d <- tbond.data$Bond_Duration[21:24]
d.tot <- 8

findVector <- function(d, d.tot) {
  objective <- function(X) {
    diff <- sum(X * d) - d.tot
    penalty <- (sum(X) - 1)^2 
    return(diff^2 + penalty)
  }
  
  set.seed(123)
  X <- runif(length(d))
  result <- optim(X, objective, method = "L-BFGS-B", lower = rep(0, length(d)), upper = rep(1, length(d)))
  return(result$par)
}


w <- findVector(d, d.tot)
data.frame(weights=w, units=(w * 100000)/tbond.data$Price[21:24],
           row.names = c("Bond21", "Bond22", "Bond23", "Bond24"))
w%*%tbond.data$Bond_Duration[21:24]

## Problem 2
r <- 0.01
sigma <- 0.1
d <- 0
S0 <- 100
k <- 1:5

#fair value forward contract
forward_prices <- S0 * exp((r-d) * k)

{plot(k, forward_prices, type = 'o', col = 'blue',
     xlab = 'Time to Maturity (k-years)', ylab = 'Forward Price',
     main = 'Forward Price vs Time to Maturity',
     xlim = c(1, 5), ylim = c(100, max(forward_prices) + 5))
grid()}

#Montecarlo simulations
n <- 10^6 

GBM <- function(k) {
  future_price <- S0 * exp((r - 0.5 * sigma^2) * k + sigma * sqrt(k) * rnorm(n))
  return(future_price)
}

simulated_prices <- matrix(NA, nrow = n, ncol = length(k))
for (j in 1:length(k)) {
    simulated_prices[,j] <- GBM(k[j])}

boxplot(simulated_prices, col = "lightblue", xlab = "Time to Maturity (k)",
        ylab = "Simulated Prices", main = "Distribution of Simulated Prices for Different Maturities")

data.frame(Expected_future_price=apply(simulated_prices, 2, FUN = mean),
           row.names=c("k=1","k=2","k=3","k=4","k=5"))


# a)
# at t0 long forward, forward price that you believe
# will be lower than S1 (index price after 1 year)
# at t1 (after 1 year) buy (-K) and sell (+S1) the stock

PnL_forward <- simulated_prices[,1] - forward_prices[1]

#b)
# at t0 you borrow money (+S0) at risk free rate,
# at t0 you buy the stock index (-S0)
# after 1 year (t1) you sell the stock (+S1) 
# and give back the money to the lender (-S0 * exp(r * 1)=-K)

PnL_underlying <- simulated_prices[,1] - (S0 * exp(r * 1))

expected_PnL_forward <- mean(PnL_forward)
VaR_forward <- expected_PnL_forward - quantile(PnL_forward, 0.05)

expected_PnL_underlying <- mean(PnL_underlying)
VaR_underlying <- expected_PnL_forward - quantile(PnL_underlying, 0.05) 
data.frame(StrategyA=c(expected_PnL_forward, VaR_forward),
           StrategyB=c(expected_PnL_underlying,VaR_underlying),
           row.names=c("Expected PnL", "VaR 95% PnL"))

## Problem 3

S0 <- 1.2273
Q <- (read.csv('FE535_Forward_Prices.csv')[['Ask']]+read.csv('FE535_Forward_Prices.csv')[['Bid']])/2
F0 <- S0 + Q/10^4
names(F0) <- read.csv('FE535_Forward_Prices.csv')[['Name']]
F0

#Theta forward-looking
theta <- log(F0/S0)*12/1:12
theta

#Theta from Libor
libor <- read.csv('FE535_Libor_USD_GBP.csv')
libor <- libor[libor$Dates=='11/13/2023',]

theta1M <- libor['US0001M.Index']/100-libor['BP0001M.Index']/100
theta3M <- libor['US0003M.Index']/100-libor['BP0003M.Index']/100
theta6M <- libor['US0006M.Index']/100-libor['BP0006M.Index']/100

theta.compare <- matrix(c(theta1M, theta3M, theta6M, theta[1], theta[3], theta[6]), ncol=2)
rownames(theta.compare)=c('Theta 1M', 'Theta 3M', 'Theta 6M')
colnames(theta.compare)=c('Libor','Forward-Looking Approach')
theta.compare

#calibrating sigma
symbol <- "GBPUSD=X"
start_date <- "2018-01-01"
end_date <- "2022-04-03"

GBPUSD <- getSymbols(symbol, from = start_date, to = end_date, src = "yahoo", auto.assign = F)
GBPUSD <- na.omit(GBPUSD)
returns <- na.omit(log(GBPUSD$`GBPUSD=X.Adjusted`/lag(GBPUSD$`GBPUSD=X.Adjusted`)))
head(returns)
sigma <- sd(returns) *  sqrt(252)

#VaR for the Unhedged
GBM <- function(n) {
  ST <- S0 * exp((theta[11] - 0.5 * sigma^2) * 11/12 + sigma * sqrt(11/12) * rnorm(n))
  return(ST)
}

ST <- GBM(10^6)

Vt <- 1.25*10^6*(ST - S0)

Expected_Vt <- mean(Vt)
VaR_Vt <- Expected_Vt - quantile(Vt, 0.01)
data.frame(PnL=c(Expected_Vt, VaR_Vt), row.names=c("Expected PnL", "VaR 99% PnL"))

#Unitary Hedge
units <- 62500
n.contracts <- 20
quantity <- units * n.contracts

#Dec2024
F0 <- as.numeric(getSymbols("6BZ24.CME", src = "yahoo", auto.assign = F)["2023-11-13"][,"6BZ24.CME.Adjusted"])
S1 <- ST
F1 <- ST*exp(theta[2]*2/12)
PnL <- quantity * (S1 - F1) - quantity * (S0 - F0)
Expected_PnL <- mean(PnL)
VaR_PnL <- Expected_PnL - quantile(PnL, 0.01)
data.frame(PnL=c(Expected_PnL, VaR_PnL), row.names=c("Expected PnL", "VaR 99% PnL"))


#Sep2024
GBM <- function(n) {
  ST <- S0 * exp((theta[10] - 0.5 * sigma^2) * 10/12 + sigma * sqrt(10/12) * rnorm(n))
  return(ST)
}

ST <- GBM(10^6)
F0 <- as.numeric(getSymbols("6BU24.CME", src = "yahoo", auto.assign = F)["2023-11-13"][,"6BU24.CME.Adjusted"])
S1 <- ST

GBM <- function(n) {
  ST <- S1 * exp((theta[1] - 0.5 * sigma^2) * 1/12 + sigma * sqrt(1/12) * rnorm(n))
  return(ST)
}

S2 <- GBM(10^6)
PnL <- quantity * (S2 - S1) - quantity * (S0 - F0)
Expected_PnL <- mean(PnL)
VaR_PnL <- Expected_PnL - quantile(PnL, 0.01)
data.frame(PnL=c(Expected_PnL, VaR_PnL), row.names=c("Expected PnL", "VaR 99% PnL"))


# Hedging using ETFs

# ETFs:
# FXB: Invesco CurrencyShares British Pound Sterling Trust
# EUO: ProShares UltraShort Euro
# DGBP: WisdomTree Bloomberg U.S. Dollar Bullish Fund
# UDN: Invesco DB U.S. Dollar Index Bearish Fund
# UUP: Invesco DB U.S. Dollar Index Bullish Fund

start_date <- "2018-01-01"
end_date <- "2022-04-03"
getSymbols('FXB', from = start_date, to = end_date)
getSymbols('EUO', from = start_date, to = end_date)
getSymbols('DGBP', from = start_date, to = end_date)
getSymbols('UDN', from = start_date, to = end_date)
getSymbols('UUP', from = start_date, to = end_date)

returns.FXB <- na.omit(log(FXB$FXB.Adjusted/lag(FXB$FXB.Adjusted)))
returns.EUO <- na.omit(log(EUO$EUO.Adjusted/lag(EUO$EUO.Adjusted)))
returns.DGBP <- na.omit(log(DGBP$DGBP.Adjusted/lag(DGBP$DGBP.Adjusted)))
returns.UDN <- na.omit(log(UDN$UDN.Adjusted/lag(UDN$UDN.Adjusted)))
returns.UUP <- na.omit(log(UUP$UUP.Adjusted/lag(UUP$UUP.Adjusted)))

returns <- merge(returns, returns.EUO, returns.UUP, returns.UDN, returns.FXB, returns.DGBP, join = 'inner')

model1 <- lm(GBPUSD.X.Adjusted ~ FXB.Adjusted, data = returns)
model2 <- lm(GBPUSD.X.Adjusted ~ EUO.Adjusted, data = returns)
model3 <- lm(GBPUSD.X.Adjusted ~ DGBP.Adjusted, data = returns)
model4 <- lm(GBPUSD.X.Adjusted ~ UDN.Adjusted, data = returns)
model5 <- lm(GBPUSD.X.Adjusted ~ UUP.Adjusted, data = returns)

# Hedge effectiveness: FXB
summary(model1)$r.squared
# Hedge effectiveness: EUO
summary(model2)$r.squared
# Hedge effectiveness: DGBP
summary(model3)$r.squared
# Hedge effectiveness: UDN
summary(model4)$r.squared
# Hedge effectiveness: UUP
summary(model5)$r.squared

## Problem 4

# Stock Price simulation: risk-neutral valuation

mu <- 0.10
sigma <- 0.15
S0 <- 120
r <- 0.03
K <- 125
t <- 1

GBM <- function(n){
  Z_seq <- rnorm(n)
  dRt_seq <- (r - sigma^2/2)*t+sigma*sqrt(t)*Z_seq
  S1 <- S0*exp(dRt_seq)
  return(list(S1, Z_seq))
}

gbm <- GBM(10^3)
S1_seq <- gbm[[1]]
Z_seq <- gbm[[2]]

plot(density(S1_seq), main = "Density of simulated stock price", xlab = "Price", ylab = "Density")

# summary

S1_exp <- mean(S1_seq)
S1_var <- var(S1_seq)
cbind(S1_exp, S1_var)


s1_exp<-S0*exp(r)
s1_var<-(exp(sigma^2)-1)*S0^2*exp(2*r)

sim_vs_true<-data.frame(Mean=c(S1_exp, s1_exp), Var=c(S1_var, s1_var))
rownames(sim_vs_true)=c('Simulation', 'True Value')
sim_vs_true


# european call price based on simulations

c_seq <- pmax(0, S1_seq - K)
c <- mean(c_seq)*exp(-r*t)
c


# european call price based on Black-Scholes model

BS <- function(S, K, r, sigma, t){
  d2 <- (log(S/K)+((r-sigma^2/2)*t))/(sigma*sqrt(t))
  d1 <- d2 + sigma*sqrt(t)
  c <- S*pnorm(d1) - K * exp(-r*t)*pnorm(d2)
  return(c)
}

c_bsm <- BS(S0, K, r, sigma, t)
c_bsm


# mean call price

set.seed(123)
M <- 100
GBM_c <- function(n){
  Z_seq <- rnorm(10^3)
  dRt_seq <- (r - sigma^2/2)*t+sigma*sqrt(t)*Z_seq
  S1 <- S0*exp(dRt_seq)
  c_seq <- pmax(0, S1 - K)
  c <- mean(c_seq)*exp(-r*t)
  c
  return(c)
}

c_vec <- sapply(1:M, GBM_c)
c_mean <- mean(c_vec)
c_mean


# MSE

squared_errors <- (c_vec - c_bsm)^2
MSE <- mean(squared_errors)
MSE

# decomposition MSE

bias <- mean(c_vec-c_bsm)^2
variance <- var(c_vec)
bias
variance
bias+variance


#Antithetic variates

Z <- c(Z_seq[1:500], -Z_seq[1:500])
dRt_seq <- (r - sigma^2/2)*t+sigma*sqrt(t)*Z
S1_seq <- S0*exp(dRt_seq)
S1_exp <- mean(S1_seq)
S1_var <- var(S1_seq)
cbind(S1_exp, S1_var)
c_seq <- pmax(0, S1_seq - K)
c <- mean(c_seq)*exp(-r*t)
c


# MSE
set.seed(123)
M <- 100
GBM_c <- function(n){
  Z_seq <- rnorm(10^3)
  Z_seq <- c(Z_seq[1:500], -Z_seq[1:500])
  dRt_seq <- (r - sigma^2/2)*t+sigma*sqrt(t)*Z_seq
  S1 <- S0*exp(dRt_seq)
  c_seq <- pmax(0, S1 - K)
  c <- mean(c_seq)*exp(-r*t)
  c
  return(c)
}

c_vec <- sapply(1:M, GBM_c)
c_mean <- mean(c_vec)
c_mean

squared_errors <- (c_vec - c_bsm)^2
MSE <- mean(squared_errors)
MSE

# decomposition MSE

bias <- mean(c_vec-c_bsm)^2
variance <- var(c_vec)
bias
variance
bias+variance

## Problem 5

## 5.1
# replicate Figure 1 of Backus et al. (1998)
us_gvt <- data.frame(Maturity=c(1,3,6,9,12,24,36,48,60,84,120),
                     Mean=c(5.314,5.640,5.884,6.003,6.079,6.272,6.386,6.467,6.531,6.624,6.683),
                     St.Dev=c(3.064,3.143,3.178,3.182,3.168,3.124,3.087,3.069,3.056,3.043,3.013),
                     Skewness=c(0.886,0.858,0.809,0.776,0.730,0.660,0.621,0.612,0.599,0.570,0.532),
                     Kurtosis=c(0.789,0.691,0.574,0.480,0.315,0.086,-0.066,-0.125,-0.200,-0.349,-0.477),
                     Auto=c(0.976,0.981,0.982,0.982,0.983,0.986,0.988,0.989,0.990,0.991,0.992))

theta <- us_gvt$Mean[1]/1200
varphi <- 0.976
lambda <- -0.0824
delta <- lambda^2/2
sigma <- sqrt((us_gvt$St.Dev[1]/1200)^2*(1-varphi^2))
# here the paper " Discrete-time models of bond pricing" 
# wrongly reports sigma=0.005560 instead of 0.0005560
B <- c()
B[1] <- 1
for (i in 2:120){
  B[i] <- 1+B[i-1]*varphi
}

A <- c()
A[1] <- 0
for (i in 2:120){
  A[i] <- A[i-1]+delta+B[i-1]*(1-varphi)*theta-((lambda+B[i-1]*sigma)^2)/2
}
A <- A[c(1,3,6,9,12,24,36,48,60,84,120)]
B <- B[c(1,3,6,9,12,24,36,48,60,84,120)]

bond.ylds_exp <- (A+B*theta)*us_gvt$Maturity^-1
bond.ylds_exp <- bond.ylds_exp * 1200

{plot(us_gvt$Maturity, us_gvt$Mean,pch = 8, col = "blue", xlab = "Maturity in months", ylab = "Mean Yield (Annual Percentage)")
lines(us_gvt$Maturity,bond.ylds_exp, type="l", col = "red")
legend("bottomright", legend = c("US T-sec yields", "Vasicek model mean yields"),
       pch = c(8, NA), col = c("blue", "red"), lty = c(0, 1))}

## 5.2
innovations <- c(0.55, -0.28, 1.78, 0.19)
sigma <- sqrt((us_gvt$St.Dev/1200)^2*(1-varphi^2))
z.t1 <- varphi*bond.ylds_exp[1]/1200 + (1-varphi)*theta+sigma[1]*innovations[1]
z.t2 <- varphi*z.t1 + (1-varphi)*theta+sigma[1]*innovations[2]
z.t3 <- varphi*z.t2 + (1-varphi)*theta+sigma[1]*innovations[3]
z.t4 <- varphi*z.t3 + (1-varphi)*theta+sigma[1]*innovations[4]

B <- c()
B[1] <- 1
for (i in 2:120){
  B[i] <- 1+B[i-1]*varphi
}

A <- c()
A[1] <- 0
for (i in 2:120){
  A[i] <- A[i-1]+delta+B[i-1]*(1-varphi)*theta-((lambda+B[i-1]*sigma)^2)/2
}
A <- A[c(1,3,6,9,12,24,36,48,60,84,120)]
B <- B[c(1,3,6,9,12,24,36,48,60,84,120)]

curve1 <- A + B*z.t1
curve2 <- A + B*z.t2
curve3 <- A + B*z.t3
curve4 <- A + B*z.t4

curve1 <- curve1*us_gvt$Maturity^-1
curve2 <- curve2*us_gvt$Maturity^-1
curve3 <- curve3*us_gvt$Maturity^-1
curve4 <- curve4*us_gvt$Maturity^-1

curve1 <- curve1*1200
curve2 <- curve2*1200
curve3 <- curve3*1200
curve4 <- curve4*1200

{plot(us_gvt$Maturity, curve1, type = "l", ylim = c(5.2, 7.5), col = "blue", lwd = 2, xlab = "Maturity", ylab = "Yields")
lines(us_gvt$Maturity, curve2, col = "red", lwd = 2)
lines(us_gvt$Maturity, curve3, col = "green", lwd = 2)
lines(us_gvt$Maturity, curve4, col = "purple", lwd = 2)
legend("bottomright", legend = c("t+1", "t+2", "t+3", "t+4"), col = c("blue", "red", "green", "purple"), lwd = 2)}
