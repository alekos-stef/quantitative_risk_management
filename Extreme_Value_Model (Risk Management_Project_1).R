## fortwsh twn dedomenwn ##
d=read.table("evt1.csv",header=TRUE,sep=";",dec=",")
d[1:10,]
X=d[[1]]; summary(X)

## suggrish dedomenwn me Normal Distribution ##
hist(X,200,prob=TRUE); rug(X);
x=seq(-10,10,0.1); lines(x,dnorm(x,mean(X),sd(X)),col="blue")
qqnorm(X); qqline(X)

## apeikonisi twn dedomenwn ##
plot(X,type="h")

##### Block-Maxima Method #####
## xwrizoume se block 20ades kai katagrafh max se kathe block ##
m=length(X); n=20; k=floor(m/n); bm=rep(0,k)
for(i in 1:k){bm[i]=max(X[((i-1)*n+1):(i*n)])}
plot(bm,type="h")

## dokimh tou xi grafika kai kataskevh tou qq-plot gia GEV ##
xi=0.00001; xi=-0.33; xi=0.35
plot(sort(bm),1/xi*((-log((1:k)/(k+1)))^(-xi)-1))

## fortwsh vivliothikhs gia thn ektimhsh twn parametrwn ths GEV ##
library(ismev)
a=gev.fit(bm)
mu=a$mle[1]; sigma=a$mle[2]; xi=a$mle[3]

## euresh qq-plot kai pp-plot xeirokinita ##
par(mfrow=c(1,2))
plot(sort(bm),1/xi*((-log((1:k)/(k+1)))^(-xi)-1)*sigma+mu)

lines(c(0,16),c(0,16),col="blue")
plot((1:k)/(k+1),exp(-(1+xi*(sort(bm)-mu)/sigma)^(-1/xi)))
lines(c(0,1),c(0,1),col="red")

## etoimh edolh gia ta parapanw grafimata ##
gev.diag(a)

## ektimhsh tou VaR ##
p=0.9999

# p=c(0.99,0.995,0.9995,0.9999)
VaR=mu-sigma/xi*(1-(-n*log(p))^(-xi)); VaR

## empeiriko VaR (deigmatiko posostimorio) ##
EVaR=sort(X)[ceiling(p*m)]
c(VaR,EVaR)

##### POT Method #####
## Euresh twn timwn pou einai panw apo u ##
u=1.5; Z0={}; peaks.i={};
for(i in 1:m){if(X[i]>u){Z0=c(Z0,X[i]);peaks.i=c(peaks.i,i)}}
Z0
peaks.i
plot(peaks.i,Z0,type="h")
lines(c(0,m),c(u,u),col="red")
Z=Z0-u
plot(peaks.i,Z,type="h")

## euresh parametrwn u,s,xi apo Generalized Pareto Distribution ##
## grafikh ektimhsh tou s, xi ##
gpd.fitrange(X,0,3,nint=31)

## epilogh tou u kai ektimhsh twn parametrwn apo mle ##
u=2
pot=gpd.fit(X,u)
xi2=pot$mle[2]; s=pot$mle[1];
s;xi2

## “epalithesush” thw protashs 9 ##
c(xi,xi2)

## grafimata prosarmoghw twn dedomenwn Z apo thn GPD ##
gpd.diag(pot)

## upologismos VaR kai ES mes POT ##
k=pot$nexc; k
p=0.9999;
VaR2=u++s/xi2*((k/(m*(1-p)))^xi2-1);VaR2
ES=(s-xi2*u+VaR2)/(1-xi2); ES

## sugkrish VaR apo tis duo methodous ##
c(VaR,VaR2)