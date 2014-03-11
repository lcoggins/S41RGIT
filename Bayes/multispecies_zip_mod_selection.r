setwd("Z:\\Dan Gwinn\\UF Files_b\\People\\Leah Beesley\\Tom")
library(rjags)
load.module('glm')
dat.1 = read.csv('BW_Data_2.csv',na.strings = "")
head(dat.1)

#------------------------------------------------------------------------------#
# This part formats the data in a vectorized form
#------------------------------------------------------------------------------#
dat = dat.1[as.numeric(is.na(dat.1$Flow))==0,]
treatment = as.numeric(as.numeric(dat$Treatment)==1 )
region = as.numeric(dat$Region)
site = as.numeric(dat$SiteCode)
year = as.numeric(dat$Year)-2010
flow = (dat$Flow-mean(dat$Flow))/sd(dat$Flow)
flow2 = flow^2
sptot = 8

catch = t(cbind(dat$C.S.F, dat$Carp, dat$Gam_hol, dat$Mcod, dat$GP, dat$Mel_flu, dat$Nem_ere, dat$Ret_sem))
samp = dim(catch)[[2]]
rmax = max(region)
#------------------------------------------------------------------------------#
# This part just looks at the data
#------------------------------------------------------------------------------#
catch[,which(treatment==0&year==0)]
catch[,which(year==0&treatment==1)]
catch[,which(year==1&treatment==0)]
catch[,which(year==1&treatment==1)]

c.1 = rowMeans(catch[,year==0&treatment==0])
t.1 = rowMeans(catch[,year==0&treatment==1])
c.2 = rowMeans(catch[,year==1&treatment==0])
t.2 = rowMeans(catch[,year==1&treatment==1])

barplot((t.2-t.1)/t.1)
barplot((c.2-c.1)/c.1)
barplot(((t.2-t.1)/t.1)-((c.2-c.1)/c.1))


par(mfrow=c(2,2))
barplot(c.1,ylim=c(0,max(catch)),main='yr-1,control')
barplot(t.1,ylim=c(0,max(catch)),main='yr-1,treatment')
barplot(c.2,ylim=c(0,max(catch)),main='yr-2,control')
barplot(t.2,ylim=c(0,max(catch)),main='yr-2,treatment')
#------------------------------------------------------------------------------#
#  R fit
#------------------------------------------------------------------------------#
library(pscl)

carp = dat$Carp
csf = dat$C.S.F
gamhol = dat$Gam_hol
mcod = dat$Mcod
gp = dat$GP
melflu = dat$Mel_flu
nemere = dat$Nem_ere
resem = dat$Ret_sem

fm = zeroinfl(resem~year*treatment+flow,dist='poisson')
summary(fm)
ft = glm(resem~year*treatment+flow, family='poisson')
AIC(fm)
AIC(ft)

summary(ft)
coef(fm)

plogis(coef(fm))[3]
#------------------------------------------------------------------------------#
# This part performs the JAGS analysis
#------------------------------------------------------------------------------#
modelFilename = "species_specific"
  cat("
model {
   for(j in 1:sptot){
    for(i in 1:8){
      w[j,i] ~ dbern(.5)
      }}
#Priors for hyperparameters
   bint.mu ~ dnorm(0,.1)
   bet1.mu ~ dnorm(0,.1)
   bet2.mu ~ dnorm(0,.1)
   bet3.mu ~ dnorm(0,.1)
   bet4.mu ~ dnorm(0,.1)
   bint.tau ~ dt(0,1,1)
   bet1.tau ~ dt(0,1,1)
   bet2.tau ~ dt(0,1,1)
   bet3.tau ~ dt(0,1,1)
   bet4.tau ~ dt(0,1,1)

   aint.mu ~ dnorm(0,.1)#dt(0,pow(1.566267,-2),7.63179)
   alph1.mu ~ dnorm(0,.1)#dt(0,pow(1.566267,-2),7.63179)
   alph2.mu ~ dnorm(0,.1)#dt(0,pow(1.566267,-2),7.63179)
   alph3.mu ~ dnorm(0,.1)#dt(0,pow(1.566267,-2),7.63179)
   alph4.mu ~ dnorm(0,.1)#dt(0,pow(1.566267,-2),7.63179)   
   aint.tau ~ dt(0,1,1)
   alph1.tau ~ dt(0,1,1)
   alph2.tau ~ dt(0,1,1)
   alph3.tau ~ dt(0,1,1)
   alph4.tau ~ dt(0,1,1)
#hyperdistributions
      for (j in 1:sptot){
   bint[j] ~ dnorm(bint.mu,bint.tau)
   bet1[j] ~ dnorm(bet1.mu,bet1.tau)
   bet2[j] ~ dnorm(bet2.mu,bet2.tau)
   bet3[j] ~ dnorm(bet3.mu,bet3.tau)
   bet4[j] ~ dnorm(bet4.mu,bet4.tau)
   aint[j]  ~ dnorm(aint.mu,aint.tau)
   alph1[j] ~ dnorm(alph1.mu,alph1.tau)
   alph2[j] ~ dnorm(alph2.mu,alph2.tau)
   alph3[j] ~ dnorm(alph3.mu,alph3.tau)
   alph4[j] ~ dnorm(alph4.mu,alph4.tau)
#model
      for (i in 1:samp){
   log.psi[j,i] <- aint[j] + w[j,1]*alph1[j]*year[i] + w[j,2]*alph2[j]*treatment[i] + w[j,3]*alph3[j]*treatment[i]*year[i] + w[j,8]*alph4[j]*ypred[j,i]
   psi[j,i] <- exp(log.psi[j,i])/(1+exp(log.psi[j,i]))

   ypred[j,i] <- exp(bint[j] + w[j,4]*bet1[j]*flow[i] + w[j,5]*bet2[j]*treatment[i] + w[j,6]*bet3[j]*year[i] + w[j,7]*bet4[j]*treatment[i]*year[i]+ epi[j,region[i]])
   z[j,i] ~ dbern(psi[j,i])
   lambda[j,i] <- z[j,i]*ypred[j,i]
   catch[j,i] ~ dpois(lambda[j,i])
           }
   tocc.1[j]  <- aint[j] + w[j,1]*alph1[j]*0 + w[j,2]*alph2[j]*1 + w[j,3]*alph3[j]*1*0
   tocc.2[j]  <- aint[j] + w[j,1]*alph1[j]*1 + w[j,2]*alph2[j]*1 + w[j,3]*alph3[j]*1*1
   cocc.1[j]  <- aint[j] + w[j,1]*alph1[j]*0 + w[j,2]*alph2[j]*0 + w[j,3]*alph3[j]*0*0
   cocc.2[j]  <- aint[j] + w[j,1]*alph1[j]*1 + w[j,2]*alph2[j]*0 + w[j,3]*alph3[j]*0*1
   tpsi.1[j]  <- exp(tocc.1[j])/(1+exp(tocc.1[j]))
   tpsi.2[j]  <- exp(tocc.2[j])/(1+exp(tocc.2[j]))
   cpsi.1[j]  <- exp(cocc.1[j])/(1+exp(cocc.1[j]))
   cpsi.2[j]  <- exp(cocc.2[j])/(1+exp(cocc.2[j]))

   tn.1[j] <- exp(bint[j] + w[j,5]*bet2[j]*1 + w[j,6]*bet3[j]*0 + w[j,7]*bet4[j]*1*0)
   tn.2[j] <- exp(bint[j] + w[j,5]*bet2[j]*1 + w[j,6]*bet3[j]*1 + w[j,7]*bet4[j]*1*1)
   cn.1[j] <- exp(bint[j] + w[j,5]*bet2[j]*0 + w[j,6]*bet3[j]*0 + w[j,7]*bet4[j]*0*0)
   cn.2[j] <- exp(bint[j] + w[j,5]*bet2[j]*0 + w[j,6]*bet3[j]*1 + w[j,7]*bet4[j]*0*1)

   treatdiff[j] <- 100*((tpsi.2[j]*tn.2[j])-(tpsi.1[j]*tn.1[j]))/(tpsi.1[j]*tn.1[j])
   controldiff[j] <- 100*((cpsi.2[j]*cn.2[j])-(cpsi.1[j]*cn.1[j]))/(cpsi.1[j]*cn.1[j])
   totaldiff[j] <- (treatdiff[j]) - (controldiff[j])
   
    for(i in 1:rmax){
        epi[j,i] ~ dnorm(0,zzepi.tau[j])
          }
        zzepi.tau[j] ~ dt(0,1,1)
   
           }
           
#Model fit
   for(j in 1:sptot){
    for(i in 1:samp){
      # generates perfect data
      zpred[j,i] ~ dbern(psi[j,i])
      cobs[j,i] ~ dpois(ypred[j,i])#dpois(z[j,i]*ypred[j,i])
      cpred[j,i] ~ dpois(ypred[j,i])#dpois(zpred[j,i]*ypred[j,i])
      # Discrepancy of observed data and model
      Socc[j,i] <- (catch[j,i]>0)-(cobs[j,i]>0)
      Scat[j,i] <- pow(catch[j,i]-lambda[j,i],2)
      # Discrepancy of perfect data and model
      Pocc[j,i] <- (cpred[j,i]>0)-(cpred[j,i]>0)
      Pcat[j,i] <- pow(cpred[j,i]-lambda[j,i],2)
      #
      flagocc[j,i] <- step(Socc[j,i]-Pocc[j,i])
      flagcat[j,i] <- step(Scat[j,i]-Pcat[j,i])
        }
      bpocc[j] <- mean(flagocc[j,])
      bpcat[j] <- mean(flagcat[j,])
      }
    bpocctot <- mean(bpocc[])
    bpcattot <- mean(bpcat[])
     }
", fill=TRUE, file=modelFilename)

#------------------------------------------------------------------------------#
# JAGS inputs
#------------------------------------------------------------------------------#

jags.data = list(flow=flow, flow2=flow2, treatment=treatment, catch=catch, sptot=sptot,  samp=samp, year=year, rmax=rmax, region=region)
jags.parms = c('totaldiff','alph3','alph4','bet4','aint','bint','bpocctot','bpcattot','w')#c('aint','alph1', 'alph2','alph3', 'aint.mu', 'alph1.mu', 'alph2.mu','alph3.mu', 'bint', 'bet1', 'bet2', 'bet3', 'bet4', 'bint.mu', 'bet1.mu', 'bet2.mu', 'bet3.mu')

jags.inits = function(){list(bint=rnorm(sptot), bet1=rnorm(sptot),bet2=rnorm(sptot),bet3=rnorm(sptot),bet4=rnorm(sptot),bet5=rnorm(sptot),
                             aint=rnorm(sptot), alph1=rnorm(sptot), alph2=rnorm(sptot),alph3=rnorm(sptot),
                             z = matrix(1,sptot,samp),

                             bint.mu = rnorm(1), bet1.mu = rnorm(1), bet2.mu = rnorm(1), bet3.mu = rnorm(1), bet4.mu = rnorm(1), bet5.mu = rnorm(1),
                             bint.tau = 1/runif(1,0,5), bet1.tau = 1/runif(1,0,5),
                             bet2.tau = 1/runif(1,0,5), bet3.tau = 1/runif(1,0,5),
                             bet4.tau = 1/runif(1,0,5), bet5.tau = 1/runif(1,0,5),

                             aint.mu = rnorm(1), alph1.mu = rnorm(1), alph2.mu = rnorm(1),alph3.mu = rnorm(1),
                             aint.tau = 1/runif(1,0,5), alph1.tau = 1/runif(1,0,5), alph2.tau = 1/runif(1,0,5), 
                             alph3.tau = 1/runif(1,0,5), alph4.tau = 1/runif(1,0,5)
                              )}
#------------------------------------------------------------------------------#
  jmod = jags.model(file=modelFilename, inits = jags.inits(),data=jags.data,  n.chains=3, n.adapt=2000)   #inits = jags.inits(),
  update(jmod, n.iter=3000, by=10, progress.bar='text')
  post = coda.samples(jmod, jags.parms, n.iter=100000, thin=100)
  mypost = as.matrix(post, chain=F)
  plot(post)
  summary(post)

  par(mfrow=c(1,2))
  barplot((c.1-c.2)-(t.1-t.2))
  barplot(colMeans(mypost))

  head(mypost,1)
  aint = mypost[,1:8]#mypost[,28:35]
  head(aint,1)
  alph3 = mypost[,9:16]#mypost[,64:71]
  head(alph3,1)
  bet4 = mypost[,17:24]
  head(bet4,1)
  bint = mypost[,25:32]
  head(bint,1)
  bpcattot = mypost[,33]
  bpocctot = mypost[,34]
  totaldiff = mypost[,35:42]
  head(totaldiff,1)
  w1 = mypost[,43:50]
  head(w1,1)
  w2 = mypost[,51:58]
  head(w2,1)
  w3 = mypost[,59:66]
  head(w3,1)
  w4 = mypost[,67:74]
  head(w4,1)
  w5 = mypost[,75:82]
  head(w5,1)
  w6 = mypost[,83:90]
  head(w6,1)
  w7 = mypost[,91:98]
  head(w7,1)
#------------------------------------------------------------------------------#
#
#------------------------------------------------------------------------------#
windows(height=3.5,width=6.5)
par(mfrow=c(1,2))
barplot(colMeans(totaldiff))
barplot(100*(((t.2-t.1)/t.1)-((c.2-c.1)/c.1)))

#------------------------------------------------------------------------------#
# Interaction probs
#------------------------------------------------------------------------------#

colMeans(w3)
colMeans(w7)
#------------------------------------------------------------------------------#
#  Model Fit
#------------------------------------------------------------------------------#

mean(bpcattot)
mean(bpocctot)   
  
#------------------------------------------------------------------------------#
#  effect graphs
#------------------------------------------------------------------------------#  
windows(height=5,width=3.5)
par(font=1, family='',mai=c(0.01,0.02,0.02,0.01),lwd=1.5)
layout(matrix(c(0,0, 0,1, 0,0), 3,2, byrow = TRUE),heights=c(.15,1,.25),widths=c(1,2))
mn = -2
mx = 8

  genname = c("C.S.F","Carp", "Gam_hol", "Mcod", "GP", "Mel_flu", "Nem_ere", "Ret_sem")
  beta = alph3*w3
  cilow = apply(beta,2,quantile,c(0.05),na.rm=T)
  cihigh = apply(beta,2,quantile,c(0.95),na.rm=T)
  position = seq(1.2,7.9,length=dim(beta)[2])

  plot(c(cilow[1],cihigh[1]),rep(position[1],2),type='l',ylim=c(1,length(genname)),xlim=c(mn,
          mx),xlab='Effect estimate',main='occupancy', yaxt='n',ylab='',xaxt='n')
          axis(1, at = c(-3,-2,-1,0,1,2,3), labels = TRUE, tick = T)
          lines(c(blow[1],bhigh[1]),rep(position[1],2)-.15,lty=2)
          points(colMeans(beta,na.rm=T),position,pch=16)
          mtext(genname[1],side=1,outer=T,line=-9.2,at=c(-2,.32),cex=.9,padj=.5,adj=1)
  cnt = 0
  for (i in 2:length(genname)){
  cnt=cnt-3.65
  lines(c(cilow[i],cihigh[i]),rep(position[i],2))
  mtext(genname[i],side=1,outer=T,line=-9.2,at=c(-2,.32),cex=.9,padj=(.5+cnt),adj=1)
  }
  abline(v=0,lty=3)
  mtext('Effect estimates',side=1,outer=T,line=-4,at=c(-2,.67),cex=.9)
#------------------------------------------------------------------------------#
#
#------------------------------------------------------------------------------#

windows(height=5,width=3.5)
par(font=1, family='',mai=c(0.01,0.02,0.02,0.01),lwd=1.5)
layout(matrix(c(0,0, 0,1, 0,0), 3,2, byrow = TRUE),heights=c(.15,1,.25),widths=c(1,2))
mn = -1.5
mx = 1.5

  genname = c("C.S.F","Carp", "Gam_hol", "Mcod", "GP", "Mel_flu", "Nem_ere", "Ret_sem")
  beta = bet4*w7
  cilow = apply(beta,2,quantile,c(0.05),na.rm=T)
  cihigh = apply(beta,2,quantile,c(0.95),na.rm=T)
  position = seq(1.2,7.9,length=dim(beta)[2])

  plot(c(cilow[1],cihigh[1]),rep(position[1],2),type='l',ylim=c(1,length(genname)),xlim=c(mn,
          mx),xlab='Effect estimate',main='catch', yaxt='n',ylab='',xaxt='n')
          axis(1, at = c(-3,-2,-1,0,1,2,3), labels = TRUE, tick = T)
          lines(c(blow[1],bhigh[1]),rep(position[1],2)-.15,lty=2)
          points(colMeans(beta,na.rm=T),position,pch=16)
          mtext(genname[1],side=1,outer=T,line=-9.2,at=c(-2,.32),cex=.9,padj=.5,adj=1)
  cnt = 0
  for (i in 2:length(genname)){
  cnt=cnt-3.65
  lines(c(cilow[i],cihigh[i]),rep(position[i],2))
  mtext(genname[i],side=1,outer=T,line=-9.2,at=c(-2,.32),cex=.9,padj=(.5+cnt),adj=1)
  }
  abline(v=0,lty=3)
  mtext('Effect estimates',side=1,outer=T,line=-4,at=c(-2,.67),cex=.9)