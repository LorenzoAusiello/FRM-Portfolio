library(zoo)
library(quantmod)
library(xts)


##daily returns
getSymbols('SPY', from = '1993-12-31', to = '2022-12-31')
head(SPY)
returns <- na.omit(log(SPY$SPY.Adjusted/lag(SPY$SPY.Adjusted)))
head(returns)


# calibrate
sigma_hat <- sqrt(252)*sd(returns)
sigma_hat
mu_hat <- 252*mean(returns)+sigma_hat^2/2
mu_hat

#simulations
s0 <- as.numeric(SPY$SPY.Adjusted[length(SPY$SPY.Adjusted)])
dt<-1/252
GBM_t <- function(n){
  dRt_seq<-rnorm(1/dt,(mu_hat - sigma_hat^2/2)*dt,sigma_hat*sqrt(dt))
  St<-s0*exp(cumsum(dRt_seq))
  return(St)
}
s_mat<-sapply(1:10^5,GBM_t)

plot(s_mat[,1], type="l",xlab="Day",ylab="Price",main="Geometric Brownian Motion" )

s1_sim<-s_mat[252,]
head(s1_sim)
F_bar<-mean(s1_sim)
F_bar
s1_exp<-s0*exp(mu_hat)
s1_exp
s1_sig<-sd(s1_sim)
s1_sig

#VaR
Qc <- quantile(s1_sim,0.05)
Qc
VaR <- F_bar - Qc
VaR
plot(density(s1_sim))
abline(v = F_bar, lty = 2)
abline(v = Qc, lty = 2, col = 2)


##monthly returns
SPY.monthly<-to.monthly(SPY)
head(SPY.monthly)
returns <- na.omit(log(SPY.monthly$SPY.Adjusted/lag(SPY.monthly$SPY.Adjusted)))
head(returns)

# calibrate
sigma_hat <- sqrt(12)*sd(returns)
sigma_hat
mu_hat <- 12*mean(returns)+sigma_hat^2/2
mu_hat

#simulations
s0 <- as.numeric(SPY.monthly$SPY.Adjusted[length(SPY.monthly$SPY.Adjusted)])
dt<-1/12
GBM_t <- function(n){
  dRt_seq<-rnorm(1/dt,(mu_hat - sigma_hat^2/2)*dt,sigma_hat*sqrt(dt))
  St<-s0*exp(cumsum(dRt_seq))
  return(St)
}
s_mat<-sapply(1:10^5,GBM_t)

plot(s_mat[,1], type="l",xlab="Day",ylab="Price",main="Geometric Brownian Motion" )

s1_sim<-s_mat[12,]
head(s1_sim)
F_bar<-mean(s1_sim)
F_bar
s1_exp<-s0*exp(mu_hat)
s1_exp
s1_sig<-sd(s1_sim)
s1_sig

#VaR
Qc <- quantile(s1_sim,0.05)
Qc
VaR <- F_bar - Qc
VaR
plot(density(s1_sim))
abline(v = F_bar, lty = 2)
abline(v = Qc, lty = 2)


## volatility increase by 5% (monthly returns)
# calibrate
sigma_hat <- sqrt(12)*(sd(returns)*1.05)
sigma_hat
mu_hat <- 12*mean(returns)+sigma_hat^2/2
mu_hat

#simulations
s0 <- as.numeric(SPY.monthly$SPY.Adjusted[length(SPY.monthly$SPY.Adjusted)])
dt<-1/12
GBM_t <- function(n){
  dRt_seq<-rnorm(1/dt,(mu_hat - sigma_hat^2/2)*dt,sigma_hat*sqrt(dt))
  St<-s0*exp(cumsum(dRt_seq))
  return(St)
}
s_mat<-sapply(1:10^5,GBM_t)

plot(s_mat[,1], type="l",xlab="Day",ylab="Price",main="Geometric Brownian Motion" )

s1_sim<-s_mat[12,]
head(s1_sim)
F_bar<-mean(s1_sim)
F_bar
s1_exp<-s0*exp(mu_hat)
s1_exp
s1_sig<-sd(s1_sim)
s1_sig

#VaR
Qc <- quantile(s1_sim,0.05)
Qc
VaR <- F_bar - Qc
VaR
plot(density(s1_sim))
abline(v = F_bar, lty = 2)
abline(v = Qc, lty = 2)


#relationship between VaR and standard deviation
Standard_deviation=c()
VaR=c()
for (i in seq(0.03,0.9,0.01)){
  sd=i
  sigma_hat <- sqrt(12)*(sd)
  mu_hat <- 12*mean(returns)+sigma_hat^2/2
  s0 <- as.numeric(SPY.monthly$SPY.Adjusted[length(SPY.monthly$SPY.Adjusted)])
  s_mat<-sapply(1:10^5,GBM_t)
  s1_sim<-s_mat[12,]
  F_bar<-mean(s1_sim)
  Standard_deviation<-append(Standard_deviation,sd)
  Qc<-quantile(s1_sim,0.05)
  VaR_value=F_bar - Qc
  VaR<- append(VaR,VaR_value)
}

plot(Standard_deviation,VaR)


#relationship between VaR (in terms of log-returns) and standard deviation
Standard_Deviation=c()
VaR=c()
for (i in seq(0.03,0.9,0.01)){
  sd=i
  sigma_hat <- sqrt(12)*(sd)
  mu_hat <- 12*mean(returns)+sigma_hat^2/2
  s0 <- as.numeric(SPY.monthly$SPY.Adjusted[length(SPY.monthly$SPY.Adjusted)])
  r1_sim<-mu_hat-sigma_hat^2/2+sigma_hat*rnorm(10^5,0,1)
  R_bar<-mean(r1_sim)
  Standard_Deviation<-append(Standard_Deviation,sd)
  Qc<-quantile(r1_sim,0.05)
  VaR_value=R_bar - Qc
  VaR<- append(VaR,VaR_value)
}

plot(Standard_Deviation,VaR, main="Standard Deviation vs VaR (log-returns) : linear relationship")
