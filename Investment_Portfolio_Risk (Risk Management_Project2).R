##### Variance-Covariance Method #####

## fortwsh twn dedomenwn ##
d=read.table("ALL.csv",header=TRUE,sep=";",dec=",")

d[1:3,]

## arithmos apodwsewn stis n hmeres ##
n=length(d$D)-1
d[c(1,n+1),]

## apodwseis thn i xrimatistiriakh hmera ##
SP500=d$SP500/d$SP500[1]-1;
AAPL=d$AAPL/d$AAPL[1]-1;
MSFT=d$MSFT/d$MSFT[1]-1;
KO=d$KO/d$KO[1]-1;

days=1:(n+1)
plot(days,SP500,type="l",ylim=c(-0.3,1))
lines(days,AAPL,type="l",col="green")
lines(days,MSFT,type="l",col="red")
lines(days,KO,type="l",col="blue")

## kovoume parathsriseis gia statherh diakumansh tou modelou ##

n0=110;
R.SP500=diff(d$SP500)/d$SP500[-(n+1)];R.SP500=R.SP500[n0:n]
R.AAPL=diff(d$AAPL)/d$AAPL[-(n+1)];R.AAPL=R.AAPL[n0:n]
R.MSFT=diff(d$MSFT)/d$MSFT[-(n+1)];R.MSFT=R.MSFT[n0:n]
R.KO=diff(d$KO)/d$KO[-(n+1)];R.KO=R.KO[n0:n]

par(mfrow=c(1,3))
plot(R.AAPL,type="l");
plot(R.MSFT,type="l");
plot(R.KO,type="l");
hist(R.MSFT)

R=data.frame(R.AAPL,R.MSFT,R.KO)
plot(R)
cor(R)
cov(R)
c(mean(R.AAPL),mean(R.MSFT),mean(R.KO))

qqnorm(R.AAPL);qqline(R.AAPL)
qqnorm(R.MSFT);qqline(R.MSFT)
qqnorm(R.KO);qqline(R.KO)

hist(R.AAPL,20);hist(R.MSFT,20);hist(R.KO,20);

## euresh parametrwn ths katanomhs tou kerdous ##

w=c(2000,4000,1000);w
S0=c(d$AAPL[n+1],d$MSFT[n+1],d$KO[n+1]);S0
u=w*S0;u
mu=matrix(c(mean(R.AAPL),mean(R.MSFT),mean(R.KO)),nrow=3
          ,ncol=1);mu

mu.profit=u%*%mu; mu.profit
sd.profit=(u%*%cov(R)%*%u)^0.5; sd.profit

p=0.995
VaR=-mu.profit+sd.profit*qnorm(p);VaR
ES=-mu.profit+sd.profit*dnorm(qnorm(p))/(1-p);ES


##### OPTIMAL PORTFOLIO #####

mu.profit=0.02; days=10;
sigma.matrix=days*cov(R); sigma.matrix
mu.vector=days*c(mean(R.AAPL),mean(R.MSFT),mean(R.KO))
mu.vector

## veltisth sunthesh gia mia xrhmatikh monada ##
unit.vector=c(1,1,1)
b11=(unit.vector%*%solve(sigma.matrix)%*%unit.vector)[1];b11
b12=(unit.vector%*%solve(sigma.matrix)%*%mu.vector)[1];b12
b22=(mu.vector%*%solve(sigma.matrix)%*%mu.vector)[1];b22
B=matrix(c(b11,b12,b12,b22),nrow=2,ncol=2);B
lambda=solve(B)%*%c(1,mu.profit);lambda
u.opt=lambda[1]*solve(sigma.matrix)%*%unit.vector+
  +lambda[2]*solve(sigma.matrix)%*%mu.vector; u.opt

## veltisth sunthesh gia k-xrhmatikes monades ##
K=10^6; 
w.opt=(u.opt*K)/S0; w.opt
mu.profit*K
sd.profit=(as.vector(u.opt)%*%sigma.matrix%*%u.opt)^(1/2);
sd.profit*K
p=0.9;
VaR=(-mu.profit+sd.profit*qnorm(p))*K;VaR
ES=(-mu.profit+sd.profit*dnorm(qnorm(p))/(1-p))*K; ES


## euresh kampilhs apotelesmatikou sunorou me elaxisth diaspora ##

par(mfrow=c(1,1))
s.opt=function(x){c(1,x)%*%solve(B)%*%c(1,x)}
m=seq(-0.02,0.07,0.001);v=rep(0,length(m))
for(i in 1:length(m)){v[i]=s.opt(m[i])}
plot(m,v,type="l",lwd=3,col="blue")

mu.star=b12/b11;mu.star
1/b11
sd.profit^2

plot(m,-m,type="l",col="red",lwd=2,ylim=c(-0.05,0.08))
VaRm=-m+v^0.5*qnorm(0.9);lines(m,VaRm,col="blue",lwd=2)
VaRm=-m+v^0.5*qnorm(0.95);lines(m,VaRm,col="blue",lwd=2)
VaRm=-m+v^0.5*qnorm(0.8);lines(m,VaRm,col="blue",lwd=2)



##### BETA COEFFICIENTS #####

R=data.frame(R.SP500,R.AAPL,R.MSFT,R.KO)
cov(R)
plot(R)

fit1=lm(R.AAPL~R.SP500);summary(fit1) 
a1=coefficients(fit1)[1];beta1=coefficients(fit1)[2];
s1=sd(residuals(fit1))
c(a1,beta1,s1)

## ektimhsh parametrwn xwris palindromidh ##



beta=cov(R.SP500,R.AAPL)/var(R.SP500)
mean(R.AAPL)-beta*mean(R.SP500)
(var(R.AAPL)-beta*cov(R.SP500,R.AAPL))^0.5

## ektimhsh parametrwn me palindromidh ##

fit2=lm(R.MSFT~R.SP500);summary(fit2)
a2=coefficients(fit2)[1];beta2=coefficients(fit2)[2];s2=sd(residuals(fit2))
fit3=lm(R.KO~R.SP500);summary(fit3)
a3=coefficients(fit3)[1];beta3=coefficients(fit3)[2];s3=sd(residuals(fit3))
c(a1,beta1,s1)
c(a2,beta2,s2)
c(a3,beta3,s3)


mu.market=mean(R.SP500);mu.market
a=c(a1,a2,a3);b=c(beta1,beta2,beta3);
mu.R=a+b*mu.market; mu.R

bb=matrix(b,nrow=3,ncol=1)%*%b;bb
S=var(R.SP500)*bb+diag(c(s1^2,s2^2,s3^2));S

## ektimhsh tou VaR kai ES apo thn methodo ##


mu.profit=u%*%mu.R; mu.profit
sd.profit=(u%*%S%*%u)^0.5; sd.profit

p=0.995
VaR=-mu.profit+sd.profit*qnorm(p);VaR
ES=-mu.profit+sd.profit*dnorm(qnorm(p))/(1-p);ES
