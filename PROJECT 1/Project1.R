
##Excercise 1

library(quantmod)
#setwd("C:/Users/loaus/OneDrive - stevens.edu/STEVENS/Intro to Financial Risk Management/Project")
setwd('C:/Users/lausiell/OneDrive - stevens.edu/STEVENS/Intro to Financial Risk Management/Project')
# Creating a function to download ETFs daily prices
Symbols <- read.csv('FE535_ETF_Final.csv')[,1]
get_tic_function <- function(x) {
  getSymbols(x, from = "2010-01-01", to ="2023-09-30",auto.assign = F)
}
P_list <- lapply(Symbols, get_tic_function)

# Creating a variable containing only Adjusted Price
get_adj_price <- function(x) x[,6]
P_adj_list <- lapply(P_list, get_adj_price)
P_adj_list

# Merge all data in a unique dataset
Prices <- Reduce(merge,P_adj_list)
subset(head(Prices),select=c(1,2,29,30))

# Calculating daily returns
R_sub <- na.omit(log(Prices/lag(Prices)))
colnames(R_sub)<-Symbols
subset(head(R_sub),select=c(1,2,29,30))

#Calculating absolute performance summary
Mean.Return.annualized <- apply(R_sub, 2, function(x)(mean(x)*252))
Mean.Return.annualized
Volatility.Annualized <- apply(R_sub, 2, function(x)(sd(x)*sqrt(252)))
Volatility.Annualized
SharpeRatio.Annualized <- (Mean.Return.annualized-0.00)/Volatility.Annualized
SharpeRatio.Annualized
Matrix_RV <- data.frame(Mean.Return.annualized, Volatility.Annualized,SharpeRatio.Annualized)
head(Matrix_RV)

# Creating a 4 x 3 summary table containing Mean Return, Volatility and Sharpe Ratio
Summary_table<-sapply(Matrix_RV, FUN= function(x)
  c( mean(x), quantile(x,.25),quantile(x,.50),quantile(x,.75)))
rownames(Summary_table)[1]<-'Mean'
Summary_table

# Plotting asset mean returns against their volatilities
library(ggplot2)
ggplot(Matrix_RV, aes(x = Volatility.Annualized, y = Mean.Return.annualized))+
  geom_point()+
  labs(x ="Volatility", y ="Mean Return") +
  ggtitle("Volatility vs. Mean Return")

ggplot(Matrix_RV, aes(x =Volatility.Annualized , y = Mean.Return.annualized)) +
  geom_point() +
  geom_smooth(method = "lm")+
  labs(x ="Volatility", y ="Mean Return") +
  ggtitle("Volatility vs. Mean Return")

#Jensen's Alpha and Market Beta
Market<-R_sub[,1]
ETFs<-R_sub[,-1]
Beta<-sapply(ETFs,FUN=function(x) cov(x,Market)/var(Market))
Jens.Alpha<-apply(ETFs,2,FUN=function(x) mean(x)*252)-Beta*mean(Market)*252
Beta
Jens.Alpha

#Treynor Ratio
Trey.Ratio<-sapply(ETFs,FUN=function(x) mean(x)*252)/Beta
Trey.Ratio

#Tracking error and Information Ratio
Track.Err<-sapply(ETFs,FUN=function(x) sqrt(var(x-Market)*252))
Track.Err

Inf.Ratio<-(sapply(ETFs,FUN=function(x) mean(x)*252)-mean(Market)*252) /Track.Err
Inf.Ratio

Matrix_Stat<-data.frame(Beta,Jens.Alpha,Trey.Ratio,Track.Err,Inf.Ratio)
head(Matrix_Stat)

# Creating a 4 x 3 summary table
Summary_table2<-sapply(Matrix_Stat, FUN= function(x)
  c( mean(x), quantile(x,.25),quantile(x,.50),quantile(x,.75)))
rownames(Summary_table2)[1]<-'Mean'
Summary_table2

#Expense vs Performance
Expenses<-read.csv('FE535_ETF_Final.csv')[,'Expense_Ratio']
names(Expenses)<-Symbols
Matrix_Stat$Expenses<-Expenses[-1]
Expenses
head(Matrix_Stat)


# Plotting Annualized Mean Return against Betas
library(ggplot2)
R_B<-data.frame(Return=Mean.Return.annualized[-1],Beta)
ggplot(data=R_B,aes(x = Beta, y = Return)) +
  geom_point(color = "red") +
  labs(x = "Beta", y = "Annualized Mean Return", title = "Annualized Return against Beta (IVV Benchmark)") +
  geom_text(aes(label = Symbols[-1]), hjust = 0.5, vjust = -0.5, size = 3) +
  geom_smooth(method = "lm", se = FALSE,col="blue")


##Excercise 2
Symbols <- c("IVV", "IYW", "IYF")
R_sub<-R_sub[,Symbols]


Mu_A<-Mean.Return.annualized[Symbols]
Sig_A<-Volatility.Annualized[Symbols]
SR_A <- SharpeRatio.Annualized[Symbols]
result <- data.frame(cbind(Mu_A,Sig_A,SR_A))
colnames(result) <- c("Mean","Volatility","SR")
round(result,3)

Sig_mat <- data.matrix(var(R_sub)*252)
Sig_mat


# function to compute portfolio metrics
w_function <- function(weights.pfolio) {
  w_vec <- matrix(weights.pfolio,3,1)
  mu_p <- t(w_vec)%*%Mu_A
  sig_p <- sqrt(t(w_vec)%*%Sig_mat%*%w_vec)
  result <- c(mu_p,sig_p)
  return(result)
}

#GMV (w_0), SR portfolios and MVEF
vec_ones <- rep(1,nrow(Sig_mat))
w_0 <- solve(Sig_mat)%*%vec_ones
w_0 <- w_0/sum(w_0)
B_mat <- solve(Sig_mat)%*%( diag(vec_ones) - vec_ones%*%t(w_0) )
w_1 <- B_mat%*%Mu_A
x<-as.numeric(t(vec_ones)%*%solve(Sig_mat)%*%(Mu_A))
SR_pfolio<-(solve(Sig_mat)%*%(Mu_A))/x
  
data.frame(w_0,w_1,SR_pfolio)
important.metrics<-sapply(data.frame(w_0,w_1,SR_pfolio),w_function)
important.metrics<-data.frame(important.metrics)
important.metrics[3,]<-important.metrics[1,]/important.metrics[2,]
rownames(important.metrics) <- c("mu","sig","SR")
important.metrics['sig','w_0']

# def function to compute the optimal portfolios (MVEF) metrics 
w_A_function <- function(A) {
  w_vec <- w_0 + (1/A)*w_1
  mu_p <- t(w_vec)%*%Mu_A
  sig_p <- sqrt(t(w_vec)%*%Sig_mat%*%w_vec)
  result <- c(mu_p,sig_p)
  return(result)
}

#define a sequence of 20 risk aversion parameters
mu_0<-t(w_0)%*%Mu_A
mu_0<-as.numeric(mu_0)
m<-seq(mu_0,2*max(Mu_A),length.out = 1000)
A_seq <-(m-t(w_0)%*%Mu_A)/(t(w_1)%*%Mu_A)
A_seq<-1/A_seq

# compute optimal portfolios (MVEF) metrics
ds_A <- t(sapply(A_seq,w_A_function))
ds_A <- data.frame(ds_A)
names(ds_A) <- c("mu_p","sig_p")
ds_A$SR <- (ds_A$mu_p)/ds_A$sig_p

#plot optimal portfolios metrics (MVEF) and highlight GMV and SR portoflios
plot(mu_p ~ sig_p,data = ds_A,
     type = "l", ylab = expression(mu[p]),
     xlab = expression(sigma[p]),
     xlim = range(ds_A$sig_p),
     ylim = range(ds_A$mu_p))
points(mu_p~sig_p,data = ds_A[which(ds_A$sig_p == important.metrics['sig','w_0']),],
       col = 1,pch = 20,cex = 1.5)
points(mu_p~sig_p,data = ds_A[which.max(ds_A$SR),],
       col = 1,pch = 20,cex = 1.5)
grid(10)

# convex combination of the two funds to derive MVEF
lambda<-seq(-1,1,by=0.001)
w_weight<-matrix(NA,2001,3)
for (i in lambda){
w_weight[which(lambda==i),]<-i*w_0+(1-i)*SR_pfolio
}

metrics_MVEF<-apply(w_weight, 1, w_function)
rownames(metrics_MVEF) <- c("mu_p","sig_p")
metrics_MVEF<-t(metrics_MVEF)
metrics_MVEF<-data.frame(metrics_MVEF)

plot(mu_p ~ sig_p,data = ds_A,
     type = "l", ylab = expression(mu[p]),
     xlab = expression(sigma[p]),
     xlim = range(ds_A$sig_p),
     ylim = range(ds_A$mu_p),col = 'blue')
lines(mu_p ~ sig_p,data = metrics_MVEF,col = 2,lty = 2,lwd = 2)

# a world with a risk free asset with return equal to 0%
lambda<-seq(-1,1,by=0.001)
w_weight<-matrix(NA,2001,3)
for (i in lambda){
  w_weight[which(lambda==i),]<-(1-i)*SR_pfolio
}

metrics_MVEF<-apply(w_weight, 1, w_function)
rownames(metrics_MVEF) <- c("mu_p","sig_p")
metrics_MVEF<-t(metrics_MVEF)
metrics_MVEF<-data.frame(metrics_MVEF)

plot(mu_p ~ sig_p,data = ds_A,
     type = "l", ylab = expression(mu[p]),
     xlab = expression(sigma[p]),
     xlim = range(ds_A$sig_p),
     ylim = range(ds_A$mu_p))
lines(mu_p ~ sig_p,data = metrics_MVEF,col = 'blue',lty = 2,lwd = 2)

plot(mu_p ~ sig_p,data = ds_A,
     type = "l", ylab = expression(mu[p]),
     xlab = expression(sigma[p]),
     xlim = c(0,max(ds_A$sig_p)),
     ylim = c(0,max(ds_A$mu_p)))
lines(mu_p ~ sig_p,data = metrics_MVEF,col = 'blue',lty = 2,lwd = 2)

#regress the portfolio mean returns on the portfolio volatility
model<-lm(mu_p ~ sig_p,metrics_MVEF)
summary(model)
important.metrics['SR','SR_pfolio']

# MVEF numerically
# Creating a seq of weights between 0 and 1
values <- seq(-1, 1, by = 0.005)

# Creating 20 thousands possible combinations of weights
valid_combinations <- list()
for (i in 1:length(values)) {
  for (j in 1:length(values)) {
    for (k in 1:length(values)) {
      weight_vector <- c(values[i], values[j], values[k])
      if (sum(weight_vector) == 1) {
        valid_combinations <- c(valid_combinations, list(weight_vector))
      }
    }
  }
}
df <- as.matrix(do.call(rbind, valid_combinations))

results <- apply(df, 1, w_function)

rownames(results) <- c("mu_p","sig_p")
results<-as.matrix(results)
results<-t(results)
results<-data.frame(results)
summary(results)

plot(mu_p ~ sig_p,data = results,
     type = "l", ylab = expression(mu[p]),
     xlab = expression(sigma[p]),
     xlim = range(results$sig_p),
     ylim = range(results$mu_p))
grid(10)

result_df <- data.frame()
unique_values <- unique(results$mu_p)

for (value in unique_values) {
  subset_df <- results[results$mu_p == value, ]
  min_value <- min(subset_df$sig_p)
  result_df <- rbind(result_df, subset_df[subset_df$sig_p == min_value, ])
}

plot(mu_p ~ sig_p,data = result_df,
     type = "l", ylab = expression(mu[p]),
     xlab = expression(sigma[p]),
     xlim = range(result_df$sig_p),
     ylim = range(result_df$mu_p))
grid(10)



##Excercise 3

# Breaking Even
simulate_game <- function() {
  options <- c("head", "tail")
  consecutive_heads <- 0
  tosses <- 0
  while (consecutive_heads < 3) {
    tosses <- tosses + 1
    if (sample(options, size = 1,prob=c(0.5,0.5))=="tail") {
      consecutive_heads <- 0
    } else {
      consecutive_heads <- consecutive_heads + 1
    }
  }
  return(tosses)
}

tosses<-c()
for (i in 1:10^5) {
    tosses <- c(tosses,simulate_game())}

k<-1/mean(tosses)
k

#Turtles

simulate_turtle_experiment <- function(n) {
  turtles <- sample(1:n)  # Random initial velocities of turtles
  groups <- list()  # List to store groups
  
  for (turtle in turtles) {
    added_to_group <- FALSE
    for (group in groups) {
      if (turtle <= group[[1]]) {
        group <- c(turtle, group)
        added_to_group <- TRUE
        break
      }
    }
    if (!added_to_group) {
      groups <- c(groups, list(turtle))
    }
  }
  
  return(length(groups))
}

average_groups_experiment <- function(N, n) {
  total_groups <- 0
  for (i in 1:N) {
    total_groups <- total_groups + simulate_turtle_experiment(n)
  }
  average_groups <- total_groups / N
  return(average_groups)
}

N <- 10000  # Number of experiments to run
n <- 10    # Number of turtles

avg_groups <- average_groups_experiment(N, n)
cat(paste("Average number of groups for", n, "turtles after", N, "experiments:", avg_groups))



#Trees
P2<-c(120,100,80)
prob<-c(0.55*0.55,0.45*0.55*2,0.45*0.45)
Exp_P2<-sum(P2*prob)
Var_P2<-sum((P2-Exp_P2)^2*prob)
print(c(Exp_P2,Var_P2))


simulated_p2 <- function(n){
  s <- 100
  for (i in 1:2){
  st<-sample(c(s+10,s-10), size=1,prob=c(0.55,0.45))
  s<-st}
  return(st)
}

simulated_p2<-sapply(1:10^5,simulated_p2)
mean(simulated_p2)
var(simulated_p2)

simulated_p10 <- function(n){
  s <- 100
  for (i in 1:10){
    st<-sample(c(s+10,s-10), size=1,prob=c(0.55,0.45))
    s<-st}
  return(st)
}

simulated_p10<-sapply(1:10^5,simulated_p10)
mean(simulated_p10)
var(simulated_p10)

##Excercise 4
getSymbols('IVV',from = "2010-01-01", to ="2023-09-30")
IVV<-to.monthly(IVV)
s0<-IVV$IVV.Adjusted[[1]]
IVV$log.returns<-log(IVV$IVV.Adjusted/lag(IVV$IVV.Adjusted))
IVV<-na.omit(IVV)
sigma_hat<-sd(IVV$log.returns)*sqrt(12)
mu_hat<- mean(IVV$log.returns)*12+sigma_hat^2/2
calibr_values<-data.frame(mu_hat,sigma_hat)
calibr_values

dt<-1/12
GBM_t <- function(n){
  dRt_seq<-rnorm(164,(mu_hat - sigma_hat^2/2)*dt,sigma_hat*sqrt(dt))
  St<-s0*exp(cumsum(dRt_seq))
  return(St)
}
s_mat<-sapply(1:10^3,GBM_t)

plot(s_mat[,1], type="l",xlab="Month",ylab="Price",main="Geometric Brownian Motion" )

s165_sim<-s_mat[164,]
head(s165_sim)
s165.exp.sim<-mean(s165_sim)
s165.exp.sim
s165_exp<-s0*exp(mu_hat*164/12)
s165_exp
s165.sig.sim<-sd(s165_sim)
s165.sig.sim
s165_sig<-sqrt((exp(sigma_hat^2*164/12)-1)*s0^2*exp(2*mu_hat*164/12))
s165_sig

sim_vs_true<-data.frame(Mean=c(s165.exp.sim,s165_exp),Sigma=c(s165.sig.sim,s165_sig))
rownames(sim_vs_true)=c('Simulation', 'True Value')
sim_vs_true

Lp_norm<-function(p){
  Lp_norm<-c()
  for (i in 1:1000){
      Lp_norm<- c(Lp_norm, sum(abs(s_mat[,i]-IVV$IVV.Adjusted)^p)^(1/p))
    }
  Lp_norm<-matrix(Lp_norm,nrow=1000)  
  Lp_norm<-data.frame(Lp_norm)
  return(Lp_norm)
}


Lp_norm_2<-Lp_norm(p=2)
Lp_norm_2

a<-sort(unlist(Lp_norm_2))[1]
i<-as.numeric(gsub('[A-z]',"",names(a)))

x<-xts(s_mat[,i],order.by=seq.Date(from = as.Date("2010-02-01"), to = as.Date("2023-09-30"), by = "months"))
plot(x, type="l",main="Price Path: Simulated vs True", pch=1, col = 'red')
lines(IVV$IVV.Adjusted, pch=2, col = 'blue')


#VaR
Qc <- quantile(s165_sim*100,0.01)
Qc
VaR <- mean(s165_sim)*100- Qc
VaR

Standard_deviation<-c()
VaR<-c()
for (i in seq(0.10,0.5,0.01)){
  sigma_hat <- i
  s_mat1<-sapply(1:10^3,GBM_t)
  s165_sim1<-s_mat1[164,]
  F_bar<-mean(s165_sim1)
  Standard_deviation<-append(Standard_deviation,sigma_hat)
  Qc<-quantile(s165_sim1,0.01)
  VaR_value=F_bar - Qc
  VaR<- append(VaR,VaR_value)
}

plot(Standard_deviation,VaR)

Qc <- quantile(IVV$log.returns,0.01)
Qc
VaR <- mean(IVV$log.returns)- Qc
VaR


s_ret<-matrix(NA,nrow=164,ncol=1000)
for (i in 1:1000){
  for (j in 2:164){
  s_ret[j,i]<-log(s_mat[j,i]/s_mat[j-1,i])}}
s_ret[1,]<-log(s_mat[1,]/s0)

Qc <- quantile(s_ret,0.01)
Qc
VaR <- mean(s_ret)- Qc
VaR

plot(density(IVV$log.returns),col='red')
lines(density(s_ret),col='blue')

