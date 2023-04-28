# This is R code for
# How Useful is F-test in Linear Regression?
# By Jae Kim

#rm(list = ls())
K=seq(1,30,3)      # Number of X variables 
T=seq(50,2000,50)  # Sample sizes
alpha = 0.05       # The level of significance

# Calculating the critical values F(K,N-K-1) distribution
CR=matrix(NA,nrow=length(T),ncol=length(K))
for(i in 1:length(T))
  for(j in 1:length(K))
CR[i,j] = qf(1-alpha, df1=K[j],df2 = T[i]-K[j]-1)

persp(T,K,CR,xlab='T', ylab='K', zlab='',
      main='5% Critical Values of the (central) F-distribution', nticks=6, col='lightblue', 
      theta = 130, phi =10,zlim = c(1,4.1),
      cex.lab=1, cex.axis=0.7,
      ticktype='detailed')

# Example
setwd("~/")
# Read the data
datamat <- read.csv("Returns.csv",header=TRUE) 
datamat[,1] <- as.Date(datamat[,1],format = '%d/%m/%Y')
m=nrow(datamat)
markets=colnames(datamat)[3:ncol(datamat)]

# Calculate the return
ret=(datamat[2:m,-c(1,2)]-datamat[1:(m-1),-c(1,2)])/datamat[1:(m-1),-c(1,2)]
# Data frame for estimation
dat=data.frame(SS=datamat$SS[2:nrow(datamat)],ret)

# Run the regression progressively increasing T and K
T= as.integer(seq(50,7345,length.out=50))
fstat=matrix(NA,nrow=length(T),ncol=length(markets))
Rsq=matrix(NA,nrow=length(T),ncol=length(markets))
CR1=matrix(NA,nrow=length(T),ncol=length(markets))
CR2=matrix(NA,nrow=length(T),ncol=length(markets))
# Do-loop for estimation
for(i in 1:length(T)){
  for(j in 1:(length(markets))){
    tem=as.formula(paste("SS~",paste(markets[1:(j)], collapse = "+")))
    M=lm(tem, data=dat[1:T[i],])
    fstat[i,j]=summary(M)$fstatistic[1]
    Rsq[i,j]=summary(M)$r.squared
    CR1[i,j]=qf(0.95,df1=j,df2=T[i]-j-1)
    CR2[i,j]=qf(0.95,df1=j,df2=T[i]-j-1,ncp=T[i]*0.05/(1-0.05))
  }}

# 3d plot for F-test statistics
persp(T,5:length(markets),fstat[,5:24],xlab='T', ylab='K', zlab='',
      main='F-test statistics', nticks=6, 
      col='lightblue', theta = 150, phi =10,
      cex.lab=1, cex.axis=0.7,
      ticktype='detailed')

# 3d plot for R-squared values
persp(T,1:length(markets),Rsq,xlab='T', ylab='K', zlab='',
      main='5% Critical Values for the F-test', nticks=5, 
      col='lightblue', theta = 150, phi =0,
      cex.lab=1, cex.axis=0.7,
      ticktype='detailed')

# 2d plot for F-test statistics
cl=c(1,rainbow(23))
par(mfrow=c(1,2))
plot(T,fstat[,1],type="l",ylim=c(0,max(fstat)),col=1,lwd=2,
     main="F-statistics", ylab="", xlab="T")
for (i in 2:length(markets)) lines(T,fstat[,i],col=cl[i],lwd=2)
legend("topleft",legend = 1:length(markets),lty=1,lwd=2,
       col=cl,cex=0.5)

# 2d plot for R-squared values
plot(T,Rsq[,1],type="l",ylim=c(0,max(Rsq)),col=1,lwd=2,
     main="R-squared values", ylab="", xlab="T")
for (i in 2:length(markets)) lines(T,Rsq[,i],col=cl[i],lwd=2)
legend("topright",legend = 1:length(markets),lty=1,lwd=2,
       col=cl,cex=0.5)


# Non-central F-distributions:
x=seq(1,40,0.01)
T=c(100,500,1000,2000)
K=5; P0=0.05

plot(x,df(x,df1=K, df2 = T[1]-K-1,ncp= T[1]*P0/(1-P0)),type="l",lwd=2,
     ylab="Density",xlab="K=5, P0 = 0.05",
     main="Non-central F-distributions with increasing T",col=1)
for(i in 2:length(T)){
  lines(x,df(x,df1=K, df2 = T[i]-K-1,ncp= T[i]*P0/(1-P0)),col=i,lwd=2)  }
legend("topright",legend = T,lty=1,lwd=2,
       col=1:6,cex=0.7)

# Critical values for non-central F-distributions
par(mfrow=c(1,1))
K=seq(1,30,3)      # Number of X variables
T=seq(50,2000,50)  # Sample sizes

CR=matrix(NA,nrow=length(T),ncol=length(K))
for(i in 1:length(T))
  for(j in 1:length(K))
    CR[i,j] = qf(1-alpha, df1=K[j],df2 = T[i]-K[j]-1,ncp=T[i]*(0.05/(1-0.05)))

# 3D plot for critical values 
persp(T,K,CR,xlab='T', ylab='K', zlab='',
      main='5% Critical Values of Non-central F-distribution (P0=0.05)', nticks=6, col='lightblue', 
      theta = 130, phi =0,
      cex.lab=1, cex.axis=0.7,
      ticktype='detailed')


